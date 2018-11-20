%{
  open Syntax

  type 'a range_spec =
    | Token of Range.t
    | Ranged of (Range.t * 'a)


  let make_range rs1 rs2 =
    let aux = function
      | Token(rng)       -> rng
      | Ranged((rng, _)) -> rng
    in
    let rng1 = aux rs1 in
    let rng2 = aux rs2 in
      Range.unite rng1 rng2


  let make_lambda rngopt args e =
    let (_, elammain) as elam =
      List.fold_right (fun arg e ->
        (Range.dummy "make_lambda", Lambda(arg, e))
      ) args e
    in
    match rngopt with
    | None      -> elam
    | Some(rng) -> (rng, elammain)


  let binary e1 op e2 =
    let rng = make_range (Ranged(e1)) (Ranged(e2)) in
    let (rngop, vop) = op in
    (rng, Apply((Range.dummy "binary", Apply((rngop, Var(vop)), e1)), e2))


  let macro_arg_cons ((rng, emain) as e) tail =
    match emain with
    | Prev(esub) -> EarlyArg(esub) :: tail
    | _          -> LateArg(e) :: tail


  let macro_binding_arg_cons (_, x) e tail =
    BindingArg(x, e) :: tail
%}

%token<Range.t> LET LETREC LETMAC DEFEQ IN LAMBDA ARROW IF THEN ELSE LPAREN RPAREN TRUE FALSE TILDE ATMARK COLON COMMA EXCLAMATION
%token<Range.t * Syntax.identifier> IDENT BINOP_AMP BINOP_BAR BINOP_EQ BINOP_LT BINOP_GT
%token<Range.t * Syntax.identifier> BINOP_TIMES BINOP_DIVIDES BINOP_PLUS BINOP_MINUS
%token<Range.t * int> INT
%token EOI

%start main
%type<Syntax.untyped_ast> main

%%

main:
  | dec=letdec; e2=main {
        let (tok1, ident, isrec, e1) = dec in
        let rng = make_range (Token(tok1)) (Ranged(e2)) in
        if isrec then
          (rng, LetRecIn(ident, e1, e2))
        else
          (rng, LetIn(ident, e1, e2))
      }
  | IN; e=exprfun; EOI { e }
;
ty:
  | ty1=tybot; ARROW; ty2=ty {
        let rng = make_range (Ranged(ty1)) (Ranged(ty2)) in
        (rng, FuncType(ty1, ty2))
      }
  | ty=tybot { ty }
;
tybot:
  | ident=IDENT {
        let (rng, s) = ident in
        let tymain =
          match s with
          | "int"  -> BaseType(IntType)
          | "bool" -> BaseType(BoolType)
          | _      -> raise (UnknownBaseType(rng, s))
        in
        (rng, tymain)
      }
  | tok1=ATMARK; ty=tybot {
        let rng = make_range (Token(tok1)) (Ranged(ty)) in
        (rng, CodeType(ty))
      }
  | LPAREN; ty=ty; RPAREN { ty }
;
ident:
  | ident=IDENT { ident }
;
ident_and_ty:
  | LPAREN; ident=IDENT; COLON; ty=ty; RPAREN { (ident, ty) }
;
letdec:
  | tok1=LET; ident_and_ty=ident_and_ty; params=list(ident_and_ty); DEFEQ; e1=exprlet {
        (tok1, ident_and_ty, false, make_lambda None params e1)
      }
  | tok1=LETREC; ident_and_ty=ident_and_ty; params=list(ident_and_ty); DEFEQ; e1=exprlet {
        (tok1, ident_and_ty, true, make_lambda None params e1)
      }
;
macroparam:
  | TILDE; ident_and_ty=ident_and_ty                                { EarlyParam(ident_and_ty) }
  | ident_and_ty=ident_and_ty                                       { LateParam(ident_and_ty) }
  | LPAREN; binder=ident_and_ty; ARROW; bindee=ident_and_ty; RPAREN { BindingParam(binder, bindee) }
;
exprlet:
  | dec=letdec; IN; e2=exprlet {
        let (tok1, ident_and_ty, isrec, e1) = dec in
        let rng = make_range (Token(tok1)) (Ranged(e2)) in
        if isrec then
          (rng, LetRecIn(ident_and_ty, e1, e2))
        else
          (rng, LetIn(ident_and_ty, e1, e2))
      }
  | tok1=LETMAC; ident=ident; EXCLAMATION; LPAREN; macparams=nonempty_list(macroparam); RPAREN; DEFEQ; e1=exprlet; IN; e2=exprlet {
        let (_, x) = ident in
        let rng = make_range (Token(tok1)) (Ranged(e2)) in
        (rng, LetMacroIn(x, macparams, e1, e2))
      }
  | tok1=IF; e0=exprlet; THEN; e1=exprlet; ELSE; e2=exprlet {
        let rng = make_range (Token(tok1)) (Ranged(e2)) in
        (rng, If(e0, e1, e2))
      }
  | e=exprfun { e }
;
exprfun:
  | tok1=LAMBDA; args=nonempty_list(ident_and_ty); ARROW; e=exprlet {
        let rng = make_range (Token(tok1)) (Ranged(e)) in
        make_lambda (Some(rng)) args e
      }
  | e=exprland { e }
;
exprland:
  | e1=exprlor; op=BINOP_AMP; e2=exprland { binary e1 op e2 }
  | e=exprlor { e }
;
exprlor:
  | e1=exprcomp; op=BINOP_BAR; e2=exprlor { binary e1 op e2 }
  | e=exprcomp { e }
;
exprcomp:
  | e1=exprtimes; op=BINOP_EQ; e2=exprcomp { binary e1 op e2 }
  | e1=exprtimes; op=BINOP_LT; e2=exprcomp { binary e1 op e2 }
  | e1=exprtimes; op=BINOP_GT; e2=exprcomp { binary e1 op e2 }
  | e=exprtimes { e }
;
exprtimes:
  | e1=exprplus; op=BINOP_TIMES; e2=exprtimes { binary e1 op e2 }
  | e1=exprplus; op=BINOP_DIVIDES; e2=exprtimes { binary e1 op e2 }
  | e=exprplus { e }
;
exprplus:
  | e1=exprapp; op=BINOP_PLUS; e2=exprplus { binary e1 op e2 }
  | e1=exprapp; op=BINOP_MINUS; e2=exprplus { binary e1 op e2 }
  | e=exprapp { e }
;
exprapp:
  | e1=exprapp; e2=exprbot {
        let rng = make_range (Ranged(e1)) (Ranged(e2)) in
        (rng, Apply(e1, e2))
      }
  | tok1=ATMARK; e=exprbot {
        let rng = make_range (Token(tok1)) (Ranged(e)) in
        (rng, Next(e))
      }
  | tok1=TILDE; e=exprbot {
        let rng = make_range (Token(tok1)) (Ranged(e)) in
        (rng, Prev(e))
      }
  | ident=ident; EXCLAMATION; LPAREN; macargs=macroargs; rng2=RPAREN {
        let (rng1, x) = ident in
        let rng = make_range (Token(rng1)) (Token(rng2)) in
        (rng, ApplyMacro(x, macargs))
      }
  | e=exprbot { e }
;
macroargs:
  | e=exprlet                                           { macro_arg_cons e [] }
  | e=exprlet; COMMA; tail=macroargs                    { macro_arg_cons e tail }
  | ident=ident; ARROW; e=exprlet                       { macro_binding_arg_cons ident e [] }
  | ident=ident; ARROW; e=exprlet COMMA; tail=macroargs { macro_binding_arg_cons ident e tail }
;
exprbot:
  | rng=TRUE { (rng, Bool(true)) }
  | rng=FALSE { (rng, Bool(false)) }
  | c=INT { let (rng, n) = c in (rng, Int(n)) }
  | ident=ident { let (rng, x) = ident in (rng, Var(x)) }
  | LPAREN; e=exprlet; RPAREN { e }
;
