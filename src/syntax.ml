
exception UnidentifiedToken of Range.t * string
exception SeeEndOfFileInComment of Range.t
exception UnknownBaseType of Range.t * string


type identifier = string


let pp_identifier ppf s =
  Format.fprintf ppf "\"%s\"" s


type base_type =
  | IntType
  | BoolType
[@@deriving show { with_path = false; } ]

type stage =
  | Stage0
  | Stage1


let pp_stage ppf = function
  | Stage0 -> Format.fprintf ppf "stage 0"
  | Stage1 -> Format.fprintf ppf "stage 1"


type untyped_ast = Range.t * untyped_ast_main
  [@printer (fun ppf (_, utastmain) -> pp_untyped_ast_main ppf utastmain)]

and untyped_ast_main =
  | Bool     of bool
  | Int      of int
  | Var      of identifier
  | Lambda   of binder * untyped_ast
  | Apply    of untyped_ast * untyped_ast
  | If       of untyped_ast * untyped_ast * untyped_ast
  | LetIn    of binder * untyped_ast * untyped_ast
  | LetRecIn of binder * untyped_ast * untyped_ast
  | Next     of untyped_ast
  | Prev     of untyped_ast
  | LetMacroIn of identifier * macro_param list * mono_type * untyped_ast * untyped_ast
  | ApplyMacro of identifier * macro_argument list

and binder = (Range.t * identifier) * mono_type

and macro_param =
  | EarlyParam   of binder
  | LateParam    of binder
  | BindingParam of binder * binder

and macro_argument =
  | EarlyArg   of untyped_ast
  | LateArg    of untyped_ast
  | BindingArg of identifier * untyped_ast

and mono_type = Range.t * mono_type_main

and mono_type_main =
  | BaseType of base_type
  | CodeType of mono_type
  | FuncType of mono_type * mono_type

and macro_param_type =
  | EarlyParamType   of mono_type
  | LateParamType    of mono_type
  | BindingParamType of mono_type * mono_type
[@@deriving show { with_path = false; } ]


let rec erase_range (_, tymain) =
  let iter = erase_range in
  let tymain =
    match tymain with
    | BaseType(_)        -> tymain
    | FuncType(ty1, ty2) -> FuncType(iter ty1, iter ty2)
    | CodeType(ty1)      -> CodeType(iter ty1)
  in
  (Range.dummy "erased", tymain)


let overwrite_range rng (_, tymain) = (rng, tymain)


type ev_value =
  | ValInt  of int
  | ValBool of bool

and ev_value_0 =
  | V0Embed     of ev_value
  | V0Closure   of identifier option * identifier * ev_ast * environment
  | V0Primitive of identifier
  | V0Next      of ev_value_1

and ev_value_1 =
  | V1Embed     of ev_value
  | V1Primitive of identifier
  | V1Symbol    of Symbol.t
  | V1Fix       of Symbol.t option * Symbol.t * ev_value_1
  | V1Apply     of ev_value_1 * ev_value_1
  | V1If        of ev_value_1 * ev_value_1 * ev_value_1

and ev_ast =
  | EvValue0    of ev_value_0
  | EvValue1    of ev_value_1
  | EvVariable  of identifier
  | EvFix       of identifier option * identifier * ev_ast
  | EvApply     of ev_ast * ev_ast
  | EvOperation of ev_ast Operation.t
  | EvIf        of ev_ast * ev_ast * ev_ast
  | EvPrev      of ev_ast
  | EvNext      of ev_ast

and environment = (ev_value_0, ev_value_1) Env.t
  [@printer (fun ppf _ -> Format.fprintf ppf "<Env>")]
[@@deriving show { with_path = false; } ]


module Acc : sig
  type 'a t
  val empty : 'a t
  val extend : 'a t -> 'a -> 'a t
  val to_list : 'a t -> 'a list
end = struct
  type 'a t = 'a list
  let empty = []
  let extend acc x = x :: acc
  let to_list acc = List.rev acc
end
