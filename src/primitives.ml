
open Syntax
open Operation


let lam2 (op2 : arity2) : ev_value_0 =
  let x1 = "%p1" in
  let x2 = "%p2" in
  V0Closure(None, x1, EvFix(None, x2, EvOperation(Arity2(op2, EvVariable(x1), EvVariable(x2)))), Env.empty)


let initial_type_environment : Typeenv.t * environment =
  let dr = Range.dummy "primitives" in
  let b = (dr, BaseType(BoolType)) in
  let i = (dr, BaseType(IntType)) in
  let ( @-> ) ty1 ty2 = (dr, FuncType(ty1, ty2)) in
  let tylogic = b @-> b @-> b in
  let tycomp = i @-> i @-> b in
  let tyarith = i @-> i @-> i in

  List.fold_left (fun (tyenv, env) (x, ty, v) ->
    let tyenv = tyenv |> Typeenv.add x (Typeenv.Primitive(ty)) in
    let env = env |> Env.add x (Env.Both(v)) in
    (tyenv, env)
  ) (Typeenv.empty, Env.empty) [
    ("&&", tylogic, lam2 Land );
    ("||", tylogic, lam2 Lor  );
    ("==", tycomp , lam2 Equal);
    ("<=", tycomp , lam2 Leq  );
    (">=", tycomp , lam2 Geq  );
    ("<" , tycomp , lam2 Lt   );
    (">" , tycomp , lam2 Gt   );
    ("*" , tyarith, lam2 Mult );
    ("/" , tyarith, lam2 Div  );
    ("+" , tyarith, lam2 Plus );
    ("-" , tyarith, lam2 Minus);
  ]


let returnB b =
  V0Embed(ValBool(b))


let getB = function
  | V0Embed(ValBool(b)) -> b
  | _                   -> failwith "getB"


let returnI n =
  V0Embed(ValInt(n))


let getI = function
  | V0Embed(ValInt(n)) -> n
  | _                  -> failwith "getI"


let eval_0_arity2 op2 v1 v2 =
  match op2 with
  | Land  -> returnB (getB v1 && getB v2)
  | Lor   -> returnB (getB v1 || getB v2)
  | Equal -> returnB (getI v1 = getI v2)
  | Leq   -> returnB (getI v1 <= getI v2)
  | Geq   -> returnB (getI v1 >= getI v2)
  | Lt    -> returnB (getI v1 < getI v2)
  | Gt    -> returnB (getI v1 > getI v2)
  | Plus  -> returnI (getI v1 + getI v2)
  | Minus -> returnI (getI v1 - getI v2)
  | Mult  -> returnI (getI v1 * getI v2)
  | Div   -> returnI (getI v1 / getI v2)


let eval_0_operation opapp =
  match opapp with
  | Arity2(op2, v1, v2) -> eval_0_arity2 op2 v1 v2
