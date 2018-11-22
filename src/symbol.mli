
type t

val generate : unit -> t

val to_identifier : t -> string

val pp : Format.formatter -> t -> unit
