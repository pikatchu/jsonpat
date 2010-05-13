
let eval prog v =
  let prog_lb = Lexing.from_string prog in
  let prog = try Parser.expr Lexer.token prog_lb
  with 
  | Parsing.Parse_error 
  | Pos.Syntax_error -> failwith "Syntax error" in
  let v = Lexer.value (Lexing.from_string v) in
  let v = JsonAst.Eflow (Flow.singleton v) in
  match Eval.program (JsonAst.add_left v prog) with
  | JsonAst.Flow x -> AstPp.sov (Flow.car x)
  | x -> AstPp.sov x
