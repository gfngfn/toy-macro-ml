
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


let show_mono_type ty =
  let rec aux isdom (_, tymain) =
    match tymain with
    | BaseType(IntType) -> "int"
    | BaseType(BoolType) -> "bool"

    | CodeType(ty1) ->
        let s = aux true ty1 in
        "@" ^ s

    | FuncType(ty1, ty2) ->
        let s1 = aux true ty1 in
        let s2 = aux false ty2 in
        let s = s1 ^ " -> " ^ s2 in
        if isdom then "(" ^ s ^ ")" else s
  in
  aux false ty


let pp_mono_type ppf ty =
  Format.fprintf ppf "%s" (show_mono_type ty)


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
