
open Syntax

type bound_to =
  | Normal  of mono_type * stage
  | Late    of mono_type
  | Bindee  of identifier * mono_type * mono_type
  | Macro   of macro_param_type list * mono_type

type t

val empty : t

val add : identifier -> bound_to -> t -> t

val find_opt : identifier -> t -> bound_to option
