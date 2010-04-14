open JsonAst

let test_l = 
  [ "[1,2]", "x :: _ -> x", "1" ;
    "{\"field\":1}", "{\"field\":x} -> x", "1" ;
    
  ]

let () =
  List.iter 
    (function (v,p,r) ->
      let ps = p in
      let p = Lexing.from_string p in
      let v = Lexing.from_string v in
      let r = Lexing.from_string r in
      let v = Lexer.value v in
      let flow = Eflow (Flow.singleton v) in
      let res = Eval.program (add_left flow (Parser.expr Lexer.token p)) in
      let res = match res with Flow x -> Flow.car x | _ -> assert false in
      let eres = Lexer.value r in
      if compare res eres <> 0 then begin 
	AstPp.print_value stderr res ;
	output_char stderr '\n' ;
	AstPp.print_value stderr eres ;
	output_char stderr '\n' ;
	failwith ("Error on test: "^ps) ;
      end
    ) test_l
