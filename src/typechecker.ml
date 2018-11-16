
open Syntax


exception UnboundVariable of Range.t * identifier
exception InvalidOccurrenceAsToStage of Range.t * identifier * stage * stage
exception InvalidMacroOccurrence of Range.t * identifier
exception NotAMacro of Range.t * identifier
exception InvalidMacroApplication of Range.t * identifier
exception MacroArgContradiction of Range.t * macro_param_type * macro_argument
exception InvalidNumberOfMacroArgs of Range.t * int * int
exception InvalidPrev of Range.t
exception InvalidNext of Range.t
exception ContradictionError of mono_type * mono_type
exception NotAFunction of Range.t * mono_type
exception NotACode of Range.t * mono_type


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


let rec aux (stg : stage) (tyenv : Typeenv.t) ((rng, utastmain) : untyped_ast) =
  match utastmain with
  | Int(_) -> (rng, BaseType(IntType))
  | Bool(_) -> (rng, BaseType(BoolType))

  | Var(x) ->
      begin
        match tyenv |> Typeenv.find_opt x with
        | None ->
            raise (UnboundVariable(rng, x))

        | Some(boundto) ->
            begin
              match boundto with
              | Typeenv.Normal((tyx, stgx)) ->
                  if stgx = stg then
                    let (_, tymain) = tyx in
                    (rng, tymain)
                  else
                    raise (InvalidOccurrenceAsToStage(rng, x, stgx, stg))

              | Typeenv.Late(ty) ->
                  begin
                    match stg with
                    | Stage1 -> ty
                    | Stage0 -> raise (InvalidOccurrenceAsToStage(rng, x, stg, Stage1))
                  end

              | Typeenv.Macro(_) ->
                  raise (InvalidMacroOccurrence(rng, x))
            end
      end

  | Lambda(((rngv, x), tydom), utast0) ->
      let tycod = aux stg (tyenv |> Typeenv.add x (Typeenv.Normal(tydom, stg))) utast0 in
      (rng, FuncType(tydom, tycod))

  | Apply(utast1, utast2) ->
      let ty1 = aux stg tyenv utast1 in
      let ty2 = aux stg tyenv utast2 in
      begin
        match ty1 with
        | (_, FuncType(tydom, tycod)) ->
            unify ty2 tydom;
            tycod

        | _ ->
            let (rng1, _) = utast1 in
            raise (NotAFunction(rng1, ty1))
      end

  | If(utast0, utast1, utast2) ->
      let ty0 = aux stg tyenv utast0 in
      unify ty0 (Range.dummy "If", BaseType(BoolType));
      let ty1 = aux stg tyenv utast1 in
      let ty2 = aux stg tyenv utast2 in
      unify ty1 ty2;
      ty1

  | LetIn(((_, x), ty1), utast1, utast2) ->
      let tyenv = tyenv |> Typeenv.add x (Typeenv.Normal(ty1, stg)) in
      let ty2 = aux stg tyenv utast2 in
      ty2

  | LetRecIn(((rngv, x), tyf), utast1, utast2) ->
      let tyenv = tyenv |> Typeenv.add x (Typeenv.Normal(tyf, stg)) in
      let ty1 = aux stg tyenv utast1 in
      unify ty1 tyf;
      let ty2 = aux stg tyenv utast2 in
      ty2

  | Prev(utast1) ->
      begin
        match stg with
        | Stage0 ->
            raise (InvalidPrev(rng))

        | Stage1 ->
            let ty1 = aux Stage0 tyenv utast1 in
            begin
              match ty1 with
              | (_, CodeType(ty)) ->
                  ty

              | _ ->
                  raise (NotACode(rng, ty1))
            end
      end

  | Next(utast1) ->
      begin
        match stg with
        | Stage1 ->
            raise (InvalidNext(rng))

        | Stage0 ->
            let ty1 = aux Stage1 tyenv utast1 in
            (rng, CodeType(ty1))
      end

  | LetMacroIn(macparams, utast1, utast2) ->
      failwith "remains to be supported"  (* TEMPORARY *)

  | ApplyMacro(x, macargs) ->
      begin
        match tyenv |> Typeenv.find_opt x with
        | None ->
            raise (UnboundVariable(rng, x))

        | Some(Typeenv.Macro(macparamtys, ty)) ->
            begin
              match stg with
              | Stage0 ->
                  raise (InvalidMacroApplication(rng, x))

              | Stage1 ->
                  aux_macro_args rng tyenv macparamtys macargs;
                  ty
            end

        | Some(_) ->
            raise (NotAMacro(rng, x))
      end


and aux_macro_args (rng : Range.t) (tyenv : Typeenv.t) (macparamtys : macro_param_type list) (macargs : macro_argument list) =
  let lenP = List.length macparamtys in
  let lenA = List.length macargs in
  let rec iter macparamtys macargs =
    match (macparamtys, macargs) with
    | ([], []) ->
        ()

    | (macparamty :: macparamtytail, macarg :: macargtail) ->
        begin
          match (macparamty, macarg) with
          | (EarlyParamType(tyP), EarlyArg(utastA)) ->
              let tyA = aux Stage0 tyenv utastA in
              unify tyP tyA

          | (LateParamType(tyP), LateArg(utastA)) ->
              let tyA = aux Stage1 tyenv utastA in
              unify tyP tyA

          | (BindingParamType(tyB, tyP), BindingArg(x, utastA)) ->
              let tyenv = tyenv |> Typeenv.add x (Typeenv.Late(tyB)) in
              let tyA = aux Stage1 tyenv utastA in
              unify tyP tyA

          | _ ->
              raise (MacroArgContradiction(rng, macparamty, macarg))
        end;
        iter macparamtytail macargtail

    | _ ->
        raise (InvalidNumberOfMacroArgs(rng, lenA, lenP))
  in
  iter macparamtys macargs


let main utast =
  let tyenv = Primitives.initial_type_environment in
  aux Stage0 tyenv utast
