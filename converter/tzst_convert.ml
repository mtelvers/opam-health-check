open Lwt.Syntax

(* One-shot migration tool: convert a legacy pixz .txz run archive to the
   framed .tzst format served by opam-health-serve. *)

let usage () =
  prerr_endline "Usage: opam-health-tzst-convert ARCHIVE.txz [ARCHIVE.tzst]";
  exit 1

let run cmd =
  let* v = Lwt_process.exec ("", Array.of_list cmd) in
  match v with
  | Unix.WEXITED 0 -> Lwt.return_unit
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      Lwt.fail (Failure ("Command failed: "^String.concat " " cmd))

let file_size file =
  let+ stat = Lwt_unix.stat file in
  stat.Unix.st_size

let () =
  let (src, dst) =
    match Sys.argv with
    | [|_; src|] when Filename.check_suffix src ".txz" ->
        (src, Filename.chop_suffix src ".txz"^".tzst")
    | [|_; src; dst|] ->
        (src, dst)
    | _ ->
        usage ()
  in
  Lwt_main.run begin
    let tmpdir = dst^".tmp" in
    let* () = Oca_lib.mkdir_p (Fpath.v tmpdir) in
    let* () = run ["tar"; "-Ipixz"; "-xf"; src; "-C"; tmpdir] in
    let* directories = Oca_lib.get_files (Fpath.v tmpdir) in
    let* () = Tzst.create ~cwd:(Fpath.v tmpdir) ~directories (Fpath.v dst) in
    let* () = Oca_lib.rm_rf (Fpath.v tmpdir) in
    let* index = Tzst.read_index (Fpath.v dst) in
    let* src_size = file_size src in
    let+ dst_size = file_size dst in
    Printf.printf "%s (%d MB) -> %s (%d MB), %d entries\n"
      src (src_size / 1_048_576) dst (dst_size / 1_048_576)
      (List.length (Tzst.entry_paths index))
  end
