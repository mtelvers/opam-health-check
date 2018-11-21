val is_valid_filename : string -> bool

val mkdir_p : Fpath.t -> unit Lwt.t

val write_line_unix : Lwt_unix.file_descr -> string -> unit Lwt.t

exception Process_failure
val exec :
  stdin:[< `Close
        | `Dev_null
        | `FD_copy of Lwt_unix.file_descr
        | `FD_move of Lwt_unix.file_descr
        | `Keep ] ->
  stdout:Lwt_unix.file_descr ->
  stderr:Lwt_unix.file_descr ->
  string list ->
  unit Lwt.t

val protocol_version : string
val default_html_port : string
val default_admin_port : string
val default_admin_name : string
val default_list_command : string
val default_ocaml_switches : Intf.Compiler.t list
val localhost : string
