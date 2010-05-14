
open JsonpatAst
module Parser = JsonpatParser
module Lexer = JsonpatLexer
module Pos = JsonpatPos
module Flow = JsonpatFlow

let eval prog v =
  let prog_lb = Lexing.from_string prog in
  let prog = try Parser.expr Lexer.token prog_lb
  with 
  | Parsing.Parse_error 
  | Pos.Syntax_error -> failwith "Syntax error" in
  let v = Lexer.value (Lexing.from_string v) in
  let v = Eflow (Flow.singleton v) in
  match JsonpatEval.program (add_left v prog) with
  | Flow x -> JsonpatAstPp.sov (Flow.car x)
  | x -> JsonpatAstPp.sov x
