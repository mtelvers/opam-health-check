module State : sig
  type t = Good | Partial | Bad | NotAvailable | InternalFailure

  val equal : t -> t -> bool

  val from_string : string -> t
  val to_string : t -> string
end

module Compiler : sig
  type t

  val from_string : string -> t
  val to_string : t -> string

  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Switch : sig
  type t

  val create : name:string -> switch:string -> t

  val name : t -> Compiler.t
  val switch : t -> string

  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Repository : sig
  type t

  val create : name:string -> github:string -> for_switches:Compiler.t list option -> t

  val name : t -> string
  val github : t -> string
  val github_user : t -> string
  val github_repo : t -> string
  val for_switches : t -> Compiler.t list option
end

module Log : sig
  type t

  val create : (unit -> string Lwt.t) -> t
end

module Instance : sig
  type t

  val create : Compiler.t -> State.t -> Log.t -> t

  val compiler : t -> Compiler.t
  val state : t -> State.t
  val content : t -> string Lwt.t
end

module Pkg : sig
  type t

  val create :
    full_name:string ->
    instances:Instance.t list ->
    maintainers:string list ->
    revdeps:int ->
    t

  val equal : t -> t -> bool
  val compare : t -> t -> int

  val full_name : t -> string
  val name : t -> string
  val version : t -> string
  val maintainers : t -> string list
  val instances : t -> Instance.t list
  val revdeps : t -> int
end

module Pkg_diff : sig
  type diff =
    | NowInstallable of State.t
    | NotAvailableAnymore of State.t
    | StatusChanged of (State.t * State.t)

  type t = {
    full_name : string;
    comp : Compiler.t;
    diff : diff;
  }
end
