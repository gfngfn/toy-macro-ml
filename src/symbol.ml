
type t = int


let current = ref 0


let generate () =
  incr current;
  !current


let to_identifier symb =
  "S#" ^ string_of_int symb


let pp ppf symb =
  Format.fprintf ppf "S#%d" symb
