
open Syntax


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


type level =
  | AppLeft
  | AppRight
  | Free


let enclose_app_left ppf lev pp =
  match lev with
  | AppLeft         -> Format.fprintf ppf "(%a)" pp ()
  | AppRight | Free -> pp ppf ()


let enclose_app_right ppf lev pp =
  match lev with
  | AppLeft | AppRight -> Format.fprintf ppf "(%a)" pp ()
  | Free               -> pp ppf ()


let pp_ev_value ppf = function
  | ValInt(n)  -> Format.fprintf ppf "%d" n
  | ValBool(b) -> Format.fprintf ppf "%B" b


let rec pp_ev_value_1 lev ppf = function
  | V1Embed(c) ->
      pp_ev_value ppf c

  | V1Symbol(symb) ->
      Symbol.pp ppf symb

  | V1Apply(v1, v2) ->
      enclose_app_left ppf lev (fun ppf () ->
        Format.fprintf ppf "%a %a"
          (pp_ev_value_1 AppRight) v1
          (pp_ev_value_1 AppLeft) v2
      )

  | V1Fix(None, sx, v0) ->
      enclose_app_right ppf lev (fun ppf () ->
        Format.fprintf ppf "fun %a -> %a"
          Symbol.pp sx
          (pp_ev_value_1 Free) v0
      )

  | V1Fix(Some(sf), sx, v0) ->
      enclose_app_right ppf lev (fun ppf () ->
        Format.fprintf ppf "fix %a fun %a -> %a"
          Symbol.pp sf
          Symbol.pp sx
          (pp_ev_value_1 Free) v0
      )

  | V1Primitive(x) ->
      Format.fprintf ppf "%s" x

  | V1If(v0, v1, v2) ->
      enclose_app_right ppf lev (fun ppf () ->
        Format.fprintf ppf "if %a then %a else %a"
          (pp_ev_value_1 Free) v0
          (pp_ev_value_1 Free) v1
          (pp_ev_value_1 Free) v2
      )


and pp_ev_value_0 lev ppf = function
  | V0Embed(c) ->
      pp_ev_value ppf c

  | V0Closure(_) ->
      Format.fprintf ppf "<fun>"

  | V0Primitive(x) ->
      Format.fprintf ppf "%s" x

  | V0Next(v1) ->
      enclose_app_left ppf lev (fun ppf () ->
        Format.fprintf ppf "@@%a" (pp_ev_value_1 AppLeft) v1
      )


let pp_ev_value_1_single = pp_ev_value_1 AppLeft


let pp_ev_value_0_single = pp_ev_value_0 AppLeft
