
open Syntax


let main fname =
  let inc = open_in fname in
  let lexbuf = Lexing.from_channel inc in
  let utast = ParserInterface.process lexbuf in
  let ty = Typechecker.main utast in
  Format.printf "%a\n" pp_untyped_ast utast;
  Format.printf "%a\n" pp_mono_type ty


let () =
  try
    Arg.parse [] main ""
  with
  | ParserInterface.Error(rng) ->
      Format.printf "%a: syntax error\n" Range.pp rng

  | UnidentifiedToken(rng, s) ->
      Format.printf "%a: unidentified token\n" Range.pp rng

  | SeeEndOfFileInComment(rng) ->
      Format.printf "%a: unclosed comment\n" Range.pp rng

  | Typechecker.UnboundVariable(rng, x) ->
      Format.printf "%a: unbound variable '%s'\n" Range.pp rng x

  | Typechecker.NotAFunction(rng, ty) ->
      Format.printf "%a: not a function; it is of type %a\n"
        Range.pp rng
        pp_mono_type ty

  | Typechecker.InvalidOccurrenceAsToStage(rng, x, stg, stgreq) ->
      Format.printf "%a: variable '%s' occurs at %a but is expected to occur at %a\n"
        Range.pp rng x pp_stage stg pp_stage stgreq

  | Typechecker.InvalidMacroOccurrence(rng, x) ->
      Format.printf "%a: variable '%s' is bound to a macro\n"
        Range.pp rng x

  | Typechecker.NotAMacro(rng, x) ->
      Format.printf "%a: variable '%s' is not a macro\n"
        Range.pp rng x

  | Typechecker.InvalidMacroApplication(rng, x) ->
      Format.printf "%a: imvalid macro application of variable '%s'"
        Range.pp rng x

  | Typechecker.MacroArgContradiction(rng, macparamty, macarg) ->
      Format.printf "%a: "
        Range.pp rng

  | Typechecker.InvalidNumberOfMacroArgs(rng, n, nreq) ->
      Format.printf "%a: the macro requires %d argument(s), but here is applied to %d argument(s)\n"
        Range.pp rng n nreq

  | Typechecker.InvalidPrev(rng) ->
      Format.printf "%a: '~ ...' occurs at stage 0\n"
        Range.pp rng

  | Typechecker.InvalidNext(rng) ->
      Format.printf "%a: '@ ...' occurs at stage 1\n"
        Range.pp rng

  | Typechecker.InvalidLetMacro(rng) ->
      Format.printf "%a: 'letmac ... = ... in ...' occurs at stage 0\n"
        Range.pp rng

  | Typechecker.ContradictionError(ty1, ty2) ->
      let (rng1, _) = ty1 in
      let (rng2, _) = ty2 in
      let (rng, ty, tyreq) =
        if Range.is_dummy rng1 then
          (rng2, ty2, ty1)
        else
          (rng1, ty1, ty2)
      in
      Format.printf "%a: this expression has type %a but is expected of type %a\n"
        Range.pp rng pp_mono_type ty pp_mono_type tyreq

  | Typechecker.NotACode(rng, ty) ->
      Format.printf "%a: this expression is expected of some code type but has type %a\n"
        Range.pp rng pp_mono_type ty

  | Typechecker.ShouldBeBound(rng, x, x1, ty1) ->
      Format.printf "%a: in order to use variable '%s', variable '%s' should be bound to a value of type %a here\n"
        Range.pp rng x x1 pp_mono_type ty1
