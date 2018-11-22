
type arity2 =
  | Land
  | Lor
  | Equal
  | Leq
  | Geq
  | Lt
  | Gt
  | Plus
  | Minus
  | Mult
  | Div
[@@deriving show { with_path = false; } ]

type 'a t =
  | Arity2 of arity2 * 'a * 'a
[@@deriving show { with_path = false; } ]


let map f = function
  | Arity2(op2, x1, x2) -> Arity2(op2, f x1, f x2)


let string_of_arity2 op2 =
  match op2 with
  | Land  -> "&&"
  | Lor   -> "||"
  | Equal -> "=="
  | Leq   -> "<="
  | Geq   -> ">="
  | Lt    -> "<"
  | Gt    -> ">"
  | Plus  -> "+"
  | Minus -> "-"
  | Mult  -> "*"
  | Div   -> "/"


let pp_arity2_rich ppf op2 =
  Format.fprintf ppf "%s" (string_of_arity2 op2)
