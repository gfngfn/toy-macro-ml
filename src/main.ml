
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

  | Typechecker.ContradictionError(ty1, ty2) ->
      let (rng1, _) = ty1 in
      Format.printf "%a: this expression has type %a but is expected of type %a\n"
        Range.pp rng1
        pp_mono_type ty1
        pp_mono_type ty2
