
open Syntax


let rec eval_0 (env : environment) (eve : ev_ast) : ev_value_0 =
  match eve with
  | EvValue0(v) ->
      v

  | EvValue1(_) ->
      failwith "EvValue1 at stage 0"

  | EvVariable(x) ->
      begin
        match env |> Env.find_opt x with
        | None ->
            failwith ("variable '" ^ x ^ "' not found")

        | Some(entry) ->
            begin
              match entry with
              | Env.V1(_)   -> failwith ("variable '" ^ x ^ "' is for stage 1")
              | Env.V0(v)   -> v
              | Env.Both(v) -> v
            end
      end

  | EvFix(fopt, x, eve1) ->
      V0Closure(fopt, x, eve1, env)

  | EvApply(eve1, eve2) ->
      let v1 = eval_0 env eve1 in
      let v2 = eval_0 env eve2 in
      begin
        match v1 with
        | V0Closure(fopt, x, eve0, env0) ->
            let env0 =
              match fopt with
              | Some(f) -> env0 |> Env.add f (Env.V0(v1))
              | None    -> env0
            in
            eval_0 (env0 |> Env.add x (Env.V0(v2))) eve0

        | _ ->
            failwith "not a stage-0 closure"
      end

  | EvOperation(opapp) ->
      let opapp0 = Operation.map (eval_0 env) opapp in
      Primitives.eval_0_operation opapp0

  | EvIf(eve0, eve1, eve2) ->
      let v0 = eval_0 env eve0 in
      begin
        match v0 with
        | V0Embed(ValBool(b)) ->
            if b then
              eval_0 env eve1
            else
              eval_0 env eve2

        | _ ->
            failwith "not a stage-0 Boolean value for an if-expression"
      end

  | EvPrev(_) ->
      failwith "Prev at stage 0"

  | EvNext(eve0) ->
      let v0 = eval_1 env eve0 in
      V0Next(v0)

  | EvRef(eve0) ->
      let v0 = eval_0 env eve0 in
      let loc = ref v0 in
      V0Location(loc)

  | EvDeref(eve0) ->
      let v0 = eval_0 env eve0 in
      begin
        match v0 with
        | V0Location(loc) -> !loc
        | _               -> failwith "not a location"
      end

  | EvAssign(eve1, eve2) ->
      let v1 = eval_0 env eve1 in
      begin
        match v1 with
        | V0Location(loc) ->
            let v2 = eval_0 env eve2 in
            loc := v2;
            V0Embed(ValUnit)

        | _ ->
            failwith "not a location"
      end


and eval_1 (env : environment) (eve : ev_ast) : ev_value_1 =
  match eve with
  | EvValue0(_) ->
      failwith "EvValue0 at stage 1"

  | EvValue1(v) ->
      v

  | EvVariable(x) ->
      begin
        match env |> Env.find_opt x with
        | None ->
            failwith ("variable '" ^ x ^ "' not found")

        | Some(entry) ->
            begin
              match entry with
              | Env.V0(_)   -> failwith ("variable '" ^ x ^ "' is for stage 0")
              | Env.V1(v)   -> v
              | Env.Both(_) -> V1Primitive(x)
            end
      end

  | EvFix(fopt, x, eve0) ->
      let (sfopt, env) =
        match fopt with
        | None ->
            (None, env)

        | Some(f) ->
            let sf = Symbol.generate () in
            (Some(sf), env |> Env.add f (Env.V1(V1Symbol(sf))))
      in
      let sx = Symbol.generate () in
      let v0 = eval_1 (env |> Env.add x (Env.V1(V1Symbol(sx)))) eve0 in
      V1Fix(sfopt, sx, v0)

  | EvApply(eve1, eve2) ->
      let v1 = eval_1 env eve1 in
      let v2 = eval_1 env eve2 in
      V1Apply(v1, v2)

  | EvOperation(opapp) ->
      failwith "EvOperation at stage 1"

  | EvIf(eve0, eve1, eve2) ->
      let v0 = eval_1 env eve0 in
      let v1 = eval_1 env eve1 in
      let v2 = eval_1 env eve2 in
      V1If(v0, v1, v2)

  | EvPrev(eve0) ->
      let v0 = eval_0 env eve0 in
      begin
        match v0 with
        | V0Next(v) -> v
        | _         -> failwith "not a V0Next"
      end

  | EvNext(_) ->
      failwith "EvNext at stage 1"

  | EvRef(eve0) ->
      let v0 = eval_1 env eve0 in
      V1Ref(v0)

  | EvDeref(eve0) ->
      let v0 = eval_1 env eve0 in
      V1Deref(v0)

  | EvAssign(eve1, eve2) ->
      let v1 = eval_1 env eve1 in
      let v2 = eval_1 env eve2 in
      V1Assign(v1, v2)


let rec unlift (v : ev_value_1) : ev_ast =
  match v with
  | V1Embed(c) ->
      EvValue0(V0Embed(c))

  | V1Symbol(symb) ->
      let x = Symbol.to_identifier symb in
      EvVariable(x)

  | V1Primitive(x) ->
      EvVariable(x)

  | V1Fix(sfopt, sx, v0) ->
      let fopt =
        match sfopt with
        | None     -> None
        | Some(sf) -> Some(Symbol.to_identifier sf)
      in
      let x = Symbol.to_identifier sx in
      let eve0 = unlift v0 in
      EvFix(fopt, x, eve0)

  | V1Apply(v1, v2) ->
      let eve1 = unlift v1 in
      let eve2 = unlift v2 in
      EvApply(eve1, eve2)

  | V1If(v0, v1, v2) ->
      let eve0 = unlift v0 in
      let eve1 = unlift v1 in
      let eve2 = unlift v2 in
      EvIf(eve0, eve1, eve2)

  | V1Ref(v0) ->
      let eve0 = unlift v0 in
      EvRef(eve0)

  | V1Deref(v0) ->
      let eve0 = unlift v0 in
      EvDeref(eve0)

  | V1Assign(v1, v2) ->
      let eve1 = unlift v1 in
      let eve2 = unlift v2 in
      EvAssign(eve1, eve2)


let main (env : environment) (eve : ev_ast) : ev_value_0 =
  let v1 = eval_1 env eve in
  Format.printf "Result1: %a\n" RichPrinting.pp_ev_value_1_single v1;
  let eve = unlift v1 in
  eval_0 env eve
