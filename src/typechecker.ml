
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
exception InvalidLetMacro of Range.t
exception ContradictionError of mono_type * mono_type
exception NotAFunction of Range.t * mono_type
exception NotACode of Range.t * mono_type
exception ShouldBeBound of Range.t * identifier * identifier * mono_type
exception NonFunctionRecursion of Range.t


let lam x eve =
  EvFix(None, x, eve)


let fixpoint f x eve =
  EvFix(Some(f), x, eve)


let make_fixpoint f xs eve =
  match xs with
  | []      -> assert false
  | x :: ys -> fixpoint f x (List.fold_right lam ys eve)


let make_application f eves =
  List.fold_left (fun eve evearg -> EvApply(eve, evearg)) (EvVariable(f)) eves


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
  | Int(n) ->
      let ty = (rng, BaseType(IntType)) in
      let eve =
        match stg with
        | Stage0 -> EvValue0(V0Embed(ValInt(n)))
        | Stage1 -> EvValue1(V1Embed(ValInt(n)))
      in
      (ty, eve)

  | Bool(b) ->
      let ty = (rng, BaseType(BoolType)) in
      let eve =
        match stg with
        | Stage0 -> EvValue0(V0Embed(ValBool(b)))
        | Stage1 -> EvValue1(V1Embed(ValBool(b)))
      in
      (ty, eve)

  | Var(x) ->
      let (ty, eve) =
        match tyenv |> Typeenv.find_opt x with
        | None ->
            raise (UnboundVariable(rng, x))

        | Some(boundto) ->
            begin
              match boundto with
              | Typeenv.Primitive(ty) ->
(*
                  let eve =
                    match stg with
                    | Stage0 -> EvValue0(V0Primitive(x))
                    | Stage1 -> EvValue1(V1Primitive(x))
                  in
*)
                  (ty, EvVariable(x))

              | Typeenv.Normal((ty, stgreq)) ->
                  if stgreq = stg then
                    let (_, tymain) = ty in
                    ((rng, tymain), EvVariable(x))
                  else
                    raise (InvalidOccurrenceAsToStage(rng, x, stg, stgreq))

              | Typeenv.Late(ty) ->
                  begin
                    match stg with
                    | Stage0 -> raise (InvalidOccurrenceAsToStage(rng, x, stg, Stage1))
                    | Stage1 -> (ty, EvPrev(EvVariable(x)))
                  end

              | Typeenv.Bindee(x1, ty1req, ty) ->
                  begin
                    match stg with
                    | Stage0 ->
                        raise (InvalidOccurrenceAsToStage(rng, x, stg, Stage1))

                    | Stage1 ->
                        begin
                          match tyenv |> Typeenv.find_opt x1 with
                          | Some(Typeenv.Normal(ty1, Stage1)) ->
                              unify ty1 ty1req;
                              (ty, EvPrev(EvApply(EvVariable(x), EvNext(EvVariable(x1)))))

                          | _ ->
                              raise (ShouldBeBound(rng, x, x1, ty1req))
                        end
                  end

              | Typeenv.Macro(_) ->
                  raise (InvalidMacroOccurrence(rng, x))
            end
      in
      (overwrite_range rng ty, eve)

  | Lambda(((rngv, x), tydom), utast0) ->
      let (tycod, eve0) = aux stg (tyenv |> Typeenv.add x (Typeenv.Normal(tydom, stg))) utast0 in
      let ty = (rng, FuncType(tydom, tycod)) in
      (ty, lam x eve0)

  | Apply(utast1, utast2) ->
      let (ty1, eve1) = aux stg tyenv utast1 in
      let (ty2, eve2) = aux stg tyenv utast2 in
      begin
        match ty1 with
        | (_, FuncType(tydom, tycod)) ->
            unify ty2 tydom;
            (overwrite_range rng tycod, EvApply(eve1, eve2))

        | _ ->
            let (rng1, _) = utast1 in
            raise (NotAFunction(rng1, ty1))
      end

  | If(utast0, utast1, utast2) ->
      let (ty0, eve0) = aux stg tyenv utast0 in
      unify ty0 (Range.dummy "If", BaseType(BoolType));
      let (ty1, eve1) = aux stg tyenv utast1 in
      let (ty2, eve2) = aux stg tyenv utast2 in
      unify ty1 ty2;
      let ty = overwrite_range rng ty1 in
      (ty, EvIf(eve0, eve1, eve2))

  | LetIn(((_, x), ty1req), utast1, utast2) ->
      let (ty1, eve1) = aux stg tyenv utast1 in
      let tyenv = tyenv |> Typeenv.add x (Typeenv.Normal(erase_range ty1, stg)) in
      let (ty2, eve2) = aux stg tyenv utast2 in
      (ty2, EvApply(lam x eve2, eve1))

  | LetRecIn(((rngv, f), ty1req), utast1, utast2) ->
      let tyenv = tyenv |> Typeenv.add f (Typeenv.Normal(erase_range ty1req, stg)) in
      let (ty1, eve1) = aux stg tyenv utast1 in
      unify ty1 ty1req;
      let (ty2, eve2) = aux stg tyenv utast2 in
      begin
        match eve1 with
        | EvFix(None, x, eve1sub) ->
            (ty2, EvApply(lam f eve2, fixpoint f x eve1sub))

        | _ ->
            raise (NonFunctionRecursion(rng))
      end

  | Prev(utast1) ->
      begin
        match stg with
        | Stage0 ->
            raise (InvalidPrev(rng))

        | Stage1 ->
            let (ty1, eve1) = aux Stage0 tyenv utast1 in
            begin
              match ty1 with
              | (_, CodeType(ty)) ->
                  (overwrite_range rng ty, EvPrev(eve1))

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
            let (ty1, eve1) = aux Stage1 tyenv utast1 in
            ((rng, CodeType(ty1)), EvNext(eve1))
      end

  | LetMacroIn(f, macparams, ty1req, utast1, utast2) ->
      begin
        match stg with
        | Stage0 ->
            raise (InvalidLetMacro(rng))

        | Stage1 ->
          let macparamtys =
            macparams |> List.map (function
              | EarlyParam((_, ty))              -> EarlyParamType(ty)
              | LateParam((_, ty))               -> LateParamType(ty)
              | BindingParam((_, ty1), (_, ty2)) -> BindingParamType(ty1, ty2)
            )
          in
          let tyenv2 = tyenv |> Typeenv.add f (Typeenv.Macro(macparamtys, ty1req)) in
          let (xacc, tyenv1) =
            List.fold_left (fun (acc, tyenv) macparam ->
              match macparam with
              | EarlyParam((ident, ty)) ->
                  let (_, x) = ident in
                  (Acc.extend acc x, tyenv |> Typeenv.add x (Typeenv.Normal(ty, Stage0)))

              | LateParam((ident, ty)) ->
                  let (_, x) = ident in
                  (Acc.extend acc x, tyenv |> Typeenv.add x (Typeenv.Late(ty)))

              | BindingParam((ident1, ty1), (ident2, ty2)) ->
                  let (_, x1) = ident1 in
                  let (_, x2) = ident2 in
                  (Acc.extend acc x2, tyenv |> Typeenv.add x2 (Typeenv.Bindee(x1, ty1, ty2)))

            ) (Acc.empty, tyenv2) macparams
          in
          let xs = Acc.to_list xacc in
          let (ty1, eve1) = aux Stage1 tyenv1 utast1 in
          unify ty1 ty1req;
          let (ty2, eve2) = aux Stage1 tyenv2 utast2 in
          let eve =
            EvPrev(EvApply(lam f (EvNext(eve2)), make_fixpoint f xs (EvNext(eve1))))
          in
          (ty2, eve)
      end

  | ApplyMacro(f, macargs) ->
      begin
        match stg with
        | Stage0 ->
            raise (InvalidMacroApplication(rng, f))

        | Stage1 ->
            begin
              match tyenv |> Typeenv.find_opt f with
              | None ->
                  raise (UnboundVariable(rng, f))

              | Some(Typeenv.Macro(macparamtys, ty)) ->
                  let evargs = aux_macro_args rng tyenv macparamtys macargs in
                  (overwrite_range rng ty, EvPrev(make_application f evargs))

              | Some(_) ->
                  raise (NotAMacro(rng, f))
            end
      end


and aux_macro_args (rng : Range.t) (tyenv : Typeenv.t) (macparamtys : macro_param_type list) (macargs : macro_argument list) =
  let lenP = List.length macparamtys in
  let lenA = List.length macargs in
  let rec iter evargacc macparamtys macargs =
    match (macparamtys, macargs) with
    | ([], []) ->
        Acc.to_list evargacc

    | (macparamty :: macparamtytail, macarg :: macargtail) ->
        let eve =
          match (macparamty, macarg) with
          | (EarlyParamType(tyP), EarlyArg(utastA)) ->
              let (tyA, eveA) = aux Stage0 tyenv utastA in
              unify tyA tyP;
              eveA

          | (LateParamType(tyP), LateArg(utastA)) ->
              let (tyA, eveA) = aux Stage1 tyenv utastA in
              unify tyA tyP;
              EvNext(eveA)

          | (BindingParamType(tyB, tyP), BindingArg(x, utastA)) ->
              let tyenv = tyenv |> Typeenv.add x (Typeenv.Late(tyB)) in
              let (tyA, eveA) = aux Stage1 tyenv utastA in
              unify tyA tyP;
              lam x (EvNext(eveA))

          | _ ->
              raise (MacroArgContradiction(rng, macparamty, macarg))
        in
        iter (Acc.extend evargacc eve) macparamtytail macargtail

    | _ ->
        raise (InvalidNumberOfMacroArgs(rng, lenA, lenP))
  in
  iter Acc.empty macparamtys macargs


let main tyenv utast =
  aux Stage1 tyenv utast
