
type ('v0, 'v1) entry =
  | V0   of 'v0
  | V1   of 'v1
  | Both of 'v0

type ('v0, 'v1) t

val empty : ('v0, 'v1) t

val add : string -> ('v0, 'v1) entry -> ('v0, 'v1) t -> ('v0, 'v1) t

val find_opt : string -> ('v0, 'v1) t -> (('v0, 'v1) entry) option
