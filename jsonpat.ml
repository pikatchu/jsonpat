(*
  Copyright (c) 2010, Mylife.com
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:
        
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the
     distribution.

  3. Neither the name of Mylife.com nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

open Flow
open Genv

let parse_prog lexbuf = 
  try  Parser.expr Lexer.token lexbuf 
  with Parsing.Parse_error -> Pos.syntax_error lexbuf

let make_lexbuf_inline env = 
  Pos.file "<cmdline>" ;
  Lexing.from_string env.prog

let make_lexbuf_file env =
  if env.prog_file = ""
  then begin 
    Printf.fprintf stderr "Error: missing program (use option -p or -f)\n" ; 
    exit 1
  end ;
  Pos.file env.prog_file ;
  Util.process_file Lexing.from_channel env.prog_file

let make_lexbuf env = 
  if env.inline
  then make_lexbuf_inline env
  else make_lexbuf_file env

let make_flow env = 
  if env.files = []
  then Flow.cat Lexer.value (Pos.file "stdin" ; Lexing.from_channel stdin)
  else Flow.cat_files Lexer.value Pos.file Lexing.from_channel env.files

let print_prog prog =
  AstPp.print_expr stdout prog ; 
  print_newline() ;
  exit 0

let show_type env flow = 
  Type.threshold := env.threshold ;
  Type.show_type flow ;
  exit 0

let make_program genv flow = 
  if genv.learn <> "" 
  then Learn.prog genv (car (flow()))
  else parse_prog (make_lexbuf genv)

let load_plugins genv = 
  Dynlink.default_available_units () ;
  try List.iter Dynlink.loadfile genv.cmxs 
  with Dynlink.Error e -> 
    Printf.fprintf stderr "Dynlink error: %s\n" (Dynlink.error_message e) ;
    exit 1

let () =
  let genv = make () in
  let flow () = make_flow genv in
  let print_result = AstPp.print_nnull_value stdout in
  if genv.show_type then show_type genv (flow()) ;
  let prog = make_program genv flow in
  if genv.print then print_prog prog ;
  load_plugins genv ;
  AstCheck.program prog ;
  let flow_val = JsonAst.Val (JsonAst.Flow (flow())) in
  match Eval.program (JsonAst.add_left flow_val prog) with
  | JsonAst.Flow x -> iter print_result x
  | x -> print_result x
