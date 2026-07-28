open Lwt.Syntax

(* Framed tar+zstd archives (.tzst).

   The archive is a regular .tar.zst stream: ustar entries sorted by name,
   ending with the usual 1024-byte zero trailer. The zstd stream however is
   cut into independent frames of at least [frame_size] uncompressed bytes,
   with frame boundaries falling on tar entry boundaries, so any single entry
   can be served by decompressing just its frame instead of the whole archive.
   An index mapping every entry to its frame and offset is embedded at the end
   of the file inside a zstd skippable frame, which standard tools ignore:
   `tar -Izstd -xf` and `zstd -dc` keep working on the whole archive.

   Index layout (text, inside the skippable frame payload):
     OHCIDX01
     F <compressed-offset> <compressed-size> <uncompressed-size>   (per frame)
     E <frame-number> <header-offset> <data-size> <path>           (per entry)
   followed by a 16-byte footer closing the file: the index length as a
   little-endian 64-bit integer, then the magic string again. Directory
   entries have a trailing '/' and a data size of 0. *)

let frame_size = 16 * 1024 * 1024
let zstd_level = "-19"
let index_magic = "OHCIDX01"
let skippable_magic = 0x184D2A50l

type entry = {
  frame_idx : int;
  header_off : int; (* offset of the 512-byte tar header inside its decompressed frame *)
  size : int; (* data size; 0 for directories *)
  path : string; (* directories end with '/' *)
}

type frame = {
  c_off : int; (* offset of the frame in the archive *)
  c_size : int;
  u_off : int; (* cumulative uncompressed offset of the frame *)
  u_size : int;
}

type index = {
  frames : frame array;
  entries : entry list; (* in tar order *)
  files : (string, entry) Hashtbl.t;
}

let empty = {frames = [||]; entries = []; files = Hashtbl.create 1}

let entry_paths index = List.map (fun entry -> entry.path) index.entries

let round512 n = (n + 511) land (lnot 511)

(* Without this, a consumer exiting before reading all of its input
   (e.g. ugrep after an error) would kill the server. *)
let ignore_sigpipe = lazy (Sys.set_signal Sys.sigpipe Sys.Signal_ignore)

let with_filter ?(exit1=false) ~timeout ~write cmd read =
  Lazy.force ignore_sigpipe;
  Lwt_process.with_process ~timeout ("", Array.of_list cmd) begin fun proc ->
    let close_quietly () = Lwt.catch (fun () -> Lwt_io.close proc#stdin) (fun _ -> Lwt.return_unit) in
    let writer =
      Lwt.catch
        (fun () ->
          let* () = write proc#stdin in
          Lwt_io.close proc#stdin)
        (function
        | Unix.Unix_error (err, _, _) when Stdlib.(=) err Unix.EPIPE -> close_quietly ()
        | Lwt_io.Channel_closed _ -> close_quietly ()
        | otherwise -> Lwt.reraise otherwise)
    in
    let* res = read proc#stdout
    and* () = writer in
    let* v = proc#close in
    match v with
    | Unix.WEXITED 0 ->
        Lwt.return res
    | Unix.WEXITED 1 when exit1 ->
        Lwt.return res
    | Unix.WEXITED n ->
        let cmd = String.concat " " cmd in
        prerr_endline ("Command '"^cmd^"' failed (exit status: "^string_of_int n^").");
        Lwt.fail (Failure "process failure")
    | Unix.WSIGNALED n | Unix.WSTOPPED n ->
        let cmd = String.concat " " cmd in
        prerr_endline ("Command '"^cmd^"' killed by a signal (n°"^string_of_int n^")");
        Lwt.fail (Failure "process failure")
  end

let write_string s oc = Lwt_io.write oc s
let read_string ic = Lwt_io.read ?count:None ic

let zstd_compress input =
  with_filter ~timeout:600. ~write:(write_string input) ["zstd"; "-q"; zstd_level] read_string

(* Lwt_io.read ~count may return fewer bytes than requested *)
let really_read ic len =
  let buf = Bytes.create len in
  let rec aux off =
    if off >= len then
      Lwt.return (Bytes.unsafe_to_string buf)
    else begin
      let* n = Lwt_io.read_into ic buf off (len - off) in
      if n = 0 then Lwt.fail (Failure "Tzst: unexpected end of file")
      else aux (off + n)
    end
  in
  aux 0

let read_range ~off ~len archive =
  Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string archive) begin fun ic ->
    let* () = Lwt_io.set_position ic (Int64.of_int off) in
    really_read ic len
  end

(* the exact decompressed size is known from the index, so read straight
   into a buffer of that size instead of growing a string chunk by chunk *)
let zstd_decompress ~u_size input =
  with_filter ~timeout:60. ~write:(write_string input) ["zstd"; "-q"; "-d"; "-c"]
    (fun ic -> really_read ic u_size)

(* ustar splits names longer than 100 bytes into a prefix and a name field,
   joined back with a '/' *)
let split_name path =
  let len = String.length path in
  if len <= 100 then
    ("", path)
  else begin
    let hi = min 155 (len - 2) in
    let lo = max 1 (len - 101) in
    let rec aux i =
      if i < lo then
        failwith ("Tzst: entry name too long: "^path)
      else if Char.equal path.[i] '/' then
        (String.sub path 0 i, String.sub path (i + 1) (len - i - 1))
      else
        aux (i - 1)
    in
    aux hi
  end

let tar_header ~is_dir ~size ~mtime path =
  let b = Bytes.make 512 '\000' in
  let put off s = Bytes.blit_string s 0 b off (String.length s) in
  let put_octal off width v = put off (Printf.sprintf "%0*o" (width - 1) v) in
  let (prefix, name) = split_name path in
  put 0 name;
  put_octal 100 8 (if is_dir then 0o755 else 0o644); (* mode *)
  put_octal 108 8 0; (* uid *)
  put_octal 116 8 0; (* gid *)
  put_octal 124 12 size;
  put_octal 136 12 mtime;
  put 148 "        "; (* checksum, computed over spaces *)
  Bytes.set b 156 (if is_dir then '5' else '0'); (* typeflag *)
  put 257 "ustar"; (* magic, NUL-terminated by the zeroed buffer *)
  put 263 "00"; (* version *)
  put 345 prefix;
  let checksum = ref 0 in
  Bytes.iter (fun c -> checksum := !checksum + Char.code c) b;
  put 148 (Printf.sprintf "%06o\000 " !checksum);
  Bytes.unsafe_to_string b

type src = {
  src_path : string;
  src_mtime : int;
  src_size : int;
  src_file : Fpath.t option; (* None for directories *)
}

let rec walk dir path_prefix =
  let* files = Oca_lib.get_files dir in
  let files = List.sort String.compare files in
  let+ srcs =
    Lwt_list.map_s begin fun file ->
      let full = Fpath.add_seg dir file in
      let* stat = Lwt_unix.stat (Fpath.to_string full) in
      let mtime = int_of_float stat.Unix.st_mtime in
      match stat.Unix.st_kind with
      | Unix.S_DIR ->
          let path = path_prefix^file^"/" in
          let+ sub = walk full path in
          {src_path = path; src_mtime = mtime; src_size = 0; src_file = None} :: sub
      | Unix.S_REG ->
          Lwt.return [{src_path = path_prefix^file; src_mtime = mtime; src_size = stat.Unix.st_size; src_file = Some full}]
      | Unix.(S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK) ->
          assert false
    end files
  in
  List.concat srcs

let walk_root cwd dir =
  let full = Fpath.add_seg cwd dir in
  let* stat = Lwt_unix.stat (Fpath.to_string full) in
  let+ sub = walk full (dir^"/") in
  {src_path = dir^"/"; src_mtime = int_of_float stat.Unix.st_mtime; src_size = 0; src_file = None} :: sub

let le32 v =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 v;
  Bytes.unsafe_to_string b

let le64 v =
  let b = Bytes.create 8 in
  Bytes.set_int64_le b 0 (Int64.of_int v);
  Bytes.unsafe_to_string b

let get_le64 s off =
  Int64.to_int (Bytes.get_int64_le (Bytes.unsafe_of_string s) off)

let index_to_string frames entries =
  let buf = Buffer.create (64 * 1024) in
  Buffer.add_string buf index_magic;
  Buffer.add_char buf '\n';
  List.iter (fun frame ->
    Buffer.add_string buf (Printf.sprintf "F %d %d %d\n" frame.c_off frame.c_size frame.u_size)
  ) frames;
  List.iter (fun entry ->
    Buffer.add_string buf (Printf.sprintf "E %d %d %d %s\n" entry.frame_idx entry.header_off entry.size entry.path)
  ) entries;
  Buffer.contents buf

let index_of_string s =
  match String.split_on_char '\n' s with
  | magic :: lines when String.equal magic index_magic ->
      let (frames, entries) =
        List.fold_left begin fun (frames, entries) line ->
          match String.split_on_char ' ' line with
          | ["F"; c_off; c_size; u_size] ->
              let u_off = match frames with
                | [] -> 0
                | frame :: _ -> frame.u_off + frame.u_size
              in
              let frame = {c_off = int_of_string c_off; c_size = int_of_string c_size; u_off; u_size = int_of_string u_size} in
              (frame :: frames, entries)
          | "E" :: frame_idx :: header_off :: size :: path ->
              let entry = {
                frame_idx = int_of_string frame_idx;
                header_off = int_of_string header_off;
                size = int_of_string size;
                path = String.concat " " path;
              } in
              (frames, entry :: entries)
          | [""] ->
              (frames, entries)
          | _ ->
              failwith "Tzst: corrupted index"
        end ([], []) lines
      in
      let entries = List.rev entries in
      let files = Hashtbl.create 10_000 in
      List.iter (fun entry -> Hashtbl.replace files entry.path entry) entries;
      {frames = Array.of_list (List.rev frames); entries; files}
  | _ ->
      failwith "Tzst: not a tzst archive (missing index)"

let compression_pool = Lwt_pool.create 8 (fun () -> Lwt.return_unit)
let max_inflight = 8

let create ~cwd ~directories archive =
  let* srcs =
    let+ srcs = Lwt_list.map_s (walk_root cwd) (List.sort String.compare directories) in
    List.concat srcs
  in
  Lwt_io.with_file ~mode:Lwt_io.Output (Fpath.to_string archive) begin fun out ->
    let buf = Buffer.create (frame_size + frame_size / 4) in
    let entries = ref [] in
    let frames = ref [] in
    let inflight = Queue.create () in
    let frame_count = ref 0 in
    let c_pos = ref 0 in
    let u_pos = ref 0 in
    let write_one () =
      let (u_size, compressed) = Queue.pop inflight in
      let* compressed = compressed in
      let frame = {c_off = !c_pos; c_size = String.length compressed; u_off = !u_pos; u_size} in
      frames := frame :: !frames;
      c_pos := !c_pos + frame.c_size;
      u_pos := !u_pos + frame.u_size;
      Lwt_io.write out compressed
    in
    let flush_frame () =
      if Buffer.length buf = 0 then
        Lwt.return_unit
      else begin
        let contents = Buffer.contents buf in
        Buffer.clear buf;
        incr frame_count;
        Queue.push (String.length contents, Lwt_pool.use compression_pool (fun () -> zstd_compress contents)) inflight;
        if Queue.length inflight >= max_inflight then write_one () else Lwt.return_unit
      end
    in
    let* () =
      Lwt_list.iter_s begin fun src ->
        entries := {frame_idx = !frame_count; header_off = Buffer.length buf; size = src.src_size; path = src.src_path} :: !entries;
        Buffer.add_string buf (tar_header ~is_dir:(Option.is_none src.src_file) ~size:src.src_size ~mtime:src.src_mtime src.src_path);
        let* () =
          match src.src_file with
          | None -> Lwt.return_unit
          | Some file ->
              let+ content = Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string file) read_string in
              if src.src_size <> String.length content then
                failwith ("Tzst: "^src.src_path^" changed size while archiving");
              Buffer.add_string buf content;
              Buffer.add_string buf (String.make (round512 src.src_size - src.src_size) '\000')
        in
        if Buffer.length buf >= frame_size then flush_frame () else Lwt.return_unit
      end srcs
    in
    (* tar end-of-archive marker, part of the last frame *)
    Buffer.add_string buf (String.make 1024 '\000');
    let* () = flush_frame () in
    let rec drain () =
      if Queue.is_empty inflight then Lwt.return_unit
      else let* () = write_one () in drain ()
    in
    let* () = drain () in
    let index = index_to_string (List.rev !frames) (List.rev !entries) in
    let payload = index ^ le64 (String.length index) ^ index_magic in
    let* () = Lwt_io.write out (le32 skippable_magic) in
    let* () = Lwt_io.write out (le32 (Int32.of_int (String.length payload))) in
    Lwt_io.write out payload
  end

let read_index archive =
  Lwt_io.with_file ~mode:Lwt_io.Input (Fpath.to_string archive) begin fun ic ->
    let* len = Lwt_io.length ic in
    let len = Int64.to_int len in
    let* footer =
      if len < 16 then Lwt.fail (Failure ("Tzst: "^Fpath.to_string archive^" is too short"))
      else begin
        let* () = Lwt_io.set_position ic (Int64.of_int (len - 16)) in
        really_read ic 16
      end
    in
    let idx_len = get_le64 footer 0 in
    if not (String.equal (String.sub footer 8 8) index_magic) || idx_len <= 0 || idx_len > len - 16 then
      Lwt.fail (Failure ("Tzst: missing index in "^Fpath.to_string archive))
    else begin
      let* () = Lwt_io.set_position ic (Int64.of_int (len - 16 - idx_len)) in
      let+ index = really_read ic idx_len in
      index_of_string index
    end
  end

(* Neighbouring log pages live in the same frame (a frame holds ~100
   entries) and crawlers walk pages in index order, so keep the last few
   decompressed frames around. The cache holds promises: concurrent requests
   for the same frame share a single decompression. *)
let cache_max_frames = 4
let frame_cache : (string * int, string Lwt.t) Hashtbl.t = Hashtbl.create 16
let frame_cache_order : (string * int) Queue.t = Queue.create ()

let get_frame ~index archive frame_idx =
  let key = (Fpath.to_string archive, frame_idx) in
  match Hashtbl.find_opt frame_cache key with
  | Some frame -> frame
  | None ->
      let frame = index.frames.(frame_idx) in
      let p =
        let* compressed = read_range ~off:frame.c_off ~len:frame.c_size archive in
        zstd_decompress ~u_size:frame.u_size compressed
      in
      Hashtbl.replace frame_cache key p;
      Queue.push key frame_cache_order;
      if Queue.length frame_cache_order > cache_max_frames then
        Hashtbl.remove frame_cache (Queue.pop frame_cache_order);
      Lwt.on_failure p (fun _ ->
        match Hashtbl.find_opt frame_cache key with
        | Some q when Stdlib.(==) q p -> Hashtbl.remove frame_cache key
        | Some _ | None -> ());
      p

let read_member ~file ~index archive =
  match Hashtbl.find_opt index.files file with
  | None ->
      Lwt.fail (Failure ("Tzst: no entry "^file^" in "^Fpath.to_string archive))
  | Some entry ->
      let+ u = get_frame ~index archive entry.frame_idx in
      String.sub u (entry.header_off + 512) entry.size

let search ~switch ~regexp ~index archive =
  let pre = switch^"/" in
  let selected = List.filter (fun entry -> String.prefix ~pre entry.path) index.entries in
  match selected with
  | [] -> Lwt.return_nil
  | first :: _ ->
      (* entries are sorted by path, so the switch's entries form a contiguous
         range of the uncompressed tar stream *)
      let last = List.hd (List.rev selected) in
      let abs entry = index.frames.(entry.frame_idx).u_off + entry.header_off in
      let start = abs first in
      let stop = abs last + 512 + round512 last.size in
      (* the overlapping frames are contiguous bytes of the archive, so the
         whole search runs as one shell pipeline with no copying in OCaml *)
      let relevant =
        List.filter (fun frame -> frame.u_off + frame.u_size > start && frame.u_off < stop)
          (Array.to_list index.frames)
      in
      let cfirst = List.hd relevant in
      let clast = List.hd (List.rev relevant) in
      Oca_lib.ugrep_tzst_range
        ~c_off:cfirst.c_off
        ~c_len:(clast.c_off + clast.c_size - cfirst.c_off)
        ~lead:(start - cfirst.u_off)
        ~range_len:(stop - start)
        ~regexp ~archive
