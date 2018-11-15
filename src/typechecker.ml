
open Syntax


exception UnboundVariable of Range.t * string
exception ContradictionError of mono_type * mono_type
exception NotAFunction of Range.t * mono_type


let unify tyact tyexp =
  let rec aux ty1 ty2 =
    let (_, ty1main) = ty1 in
    let (_, ty2main) = ty2 in
    match (ty1main, ty2main) with
    | (BaseType(bt1), BaseType(bt2)) -> bt1 = bt2

    | (FuncType(ty1d, ty1c), FuncType(ty2d, ty2c)) ->
        let res1 = aux ty1d ty2d in
        let res2 = aux ty1c ty2c in
        res1 && res2

    | (CodeType(ty1), CodeType(ty2)) -> aux ty1 ty2

    | _ -> false
  in
  let res = aux tyact tyexp in
  if res then () else raise (ContradictionError(tyact, tyexp))


let rec aux lev tyenv (rng, utastmain) =
  match utastmain with
  | Int(_) -> (rng, BaseType(IntType))
  | Bool(_) -> (rng, BaseType(BoolType))

  | Var(x) ->
      begin
        match tyenv |> Typeenv.find_opt x with
        | None              -> raise (UnboundVariable(rng, x))
        | Some((_, tymain)) -> (rng, tymain)
      end

  | Lambda(((rngv, x), tydom), utast0) ->
      let tycod = aux lev (tyenv |> Typeenv.add x tydom) utast0 in
      (rng, FuncType(tydom, tycod))

  | Apply(utast1, utast2) ->
      let ty1 = aux lev tyenv utast1 in
      let ty2 = aux lev tyenv utast2 in
      begin
        match ty2 with
        | (_, FuncType(tydom, tycod)) ->
            unify ty1 tydom;
            tycod

        | _ ->
            let (rng1, _) = utast1 in
            raise (NotAFunction(rng1, ty1))
      end

  | If(utast0, utast1, utast2) ->
      let ty0 = aux lev tyenv utast0 in
      unify ty0 (Range.dummy "If", BaseType(BoolType));
      let ty1 = aux lev tyenv utast1 in
      let ty2 = aux lev tyenv utast2 in
      unify ty1 ty2;
      ty1

  | LetIn(((_, x), ty1), utast1, utast2) ->
      let ty2 = aux lev (tyenv |> Typeenv.add x ty1) utast2 in
      ty2

  | LetRecIn(((rngv, x), tyf), utast1, utast2) ->
      let tyenv = tyenv |> Typeenv.add x tyf in
      let ty1 = aux (lev + 1) tyenv utast1 in
      unify ty1 tyf;
      let ty2 = aux lev tyenv utast2 in
      ty2


let main utast =
  let tyenv = Primitives.initial_type_environment in
  aux 0 tyenv utast
