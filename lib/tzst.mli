(** Framed tar+zstd archives (.tzst): a standard .tar.zst stream cut into
    independent zstd frames at tar entry boundaries, with an embedded index,
    so single entries can be served without decompressing the whole archive.
    Standard tools still work on the file (e.g. [tar -Izstd -xf]). *)

type index

val empty : index

(** All entry paths, in tar order; directories end with ['/']. *)
val entry_paths : index -> string list

(** [create ~cwd ~directories archive] archives the given directories
    (relative to [cwd]), sorted by name, into [archive]. *)
val create : cwd:Fpath.t -> directories:string list -> Fpath.t -> unit Lwt.t

val read_index : Fpath.t -> index Lwt.t

(** [read_member ~file ~index archive] returns the content of the entry
    [file] by decompressing only the frame containing it. *)
val read_member : file:string -> index:index -> Fpath.t -> string Lwt.t

(** [search ~switch ~regexp ~index archive] lists the entries under
    [switch]/ whose content matches [regexp], by streaming that switch's
    frames through ugrep. *)
val search : switch:string -> regexp:string -> index:index -> Fpath.t -> string list Lwt.t
