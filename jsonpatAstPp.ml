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

(*****************************************************************************)
(* Jsonpat Ast pretty-printer                                                *)
(* All th brackets are kept explicit (in case of a doubt in operators        *)
(* priorities)                                                               *)
(*****************************************************************************)
open JsonpatUtil
open JsonpatAst
open Printf

let bop = function
  | Plus  -> " + "  | Minus -> " - "  | Mult -> " * "  | Div   -> " / "
  | Eq    -> " == " | Diff  -> " <> " | Lt   -> " < "  | Gt    -> " > "
  | Lte   -> " <= " | Gte   -> " >= " | Seq  -> " >> " | Apply -> " "    
  | Cons  -> " :: " | Dot   -> "."    | Bar  -> " | "  | As    -> " as "   
  | Def -> " = "    | And   -> " && " | Or   -> " || " 

let type_ = function 
  | Tint    -> "int"    | Tbool   -> "bool"   | Tfloat -> "float" 
  | Tstring -> "string" | Tobject -> "object" | Tarray -> "array" 
  | Tany    -> "_"

let char buf = function
  | '\"'    -> o buf "\\\"" 
  | '\t'   -> o buf "\\t"
  | '\r'   -> o buf "\\r"
  | '\b'   -> o buf "\\b"
  | '\n'   -> o buf "\\n"
  | '\012' -> o buf "\\f"
  | '\\'   -> o buf "\\\\"
  | '\x00'..'\x1F'
  | '\x7F' as c -> bprintf buf "\\u%04X" (int_of_char c)
  | c      -> oc buf c

let special_float = function
  | "inf"  -> "Infinity"
  | "-inf" -> "-Infinity"
  | "nan"  -> "NaN" 
  | x      -> x

let float buf f =
  let s = string_of_float f in
  let s = special_float s in
  o buf s ;
  match s.[String.length s - 1] with
  | '.' -> oc buf '0'
  | _   -> ()

let string buf s = oc buf '"' ; String.iter (char buf) s ; oc buf '"'

let rec array buf f = function
  | []  -> ()
  | [x] -> f buf x
  | x :: rl -> f buf x ; oc buf ',' ; array buf f rl

let tuple buf f a = oc buf '(' ; array buf f a ; oc buf ')'
let array buf f a = oc buf '[' ; array buf f a ; oc buf ']'
    
let rec record buf f = function
  | [] -> ()
  | [x] -> f buf x
  | x :: rl -> f buf x ; oc buf ',' ; record buf f rl

let record buf f r = oc buf '{' ; record buf f r ; oc buf '}'

let rec value buf = function 
  | Object obj    -> record buf value_field (elements obj)
  | Array a       -> array buf value a
  | Tuple t       -> tuple buf value t
  | Variant (n,v) -> 
      o buf "<\"" ; 
      o buf n ; o buf "\":" ; value buf v ; 
      o buf ">"
  | Bool b when b -> o buf "true"
  | Bool _        -> o buf "false"
  | Int i         -> o buf (Big_int.string_of_big_int i)
  | Float f       -> float buf f
  | String s      -> string buf s
  | Flow _        -> o buf "flow" 
  | Closure _     -> o buf "closure"
  | Prim Group    -> o buf "group"
  | Prim Flatten  -> o buf "flatten"
  | Prim (Fold e) -> o buf "(flatten " ; value buf e ; oc buf ')'
  | Prim (Filter e)-> o buf "(filter " ; value buf e ; oc buf ')'
  | Prim (Drop e) -> o buf "(drop " ; value buf e ; oc buf ')'
  | Prim (Head e) -> o buf "(head " ; value buf e ; oc buf ')'
  | Pfailed       -> o buf "pattern_failure"
  | Null          -> o buf "null"

and value_field buf (x,y) = string buf x ; oc buf ':' ; value buf y

and expr buf = function
  | Any     -> oc buf '_'
  | Type ty -> o buf (type_ ty)
  | Id s    -> o buf s
  | Ebool b when b -> o buf "true"
  | Ebool _        -> o buf "false"
  | Eint i         -> o buf (Big_int.string_of_big_int i)
  | Efloat f       -> float buf f
  | Estring s      -> string buf s
  | Eflow _        -> o buf "flow" 
  | Eprim Group    -> o buf "group"
  | Eprim Flatten  -> o buf "flatten"
  | Eprim (Fold e) -> o buf "(flatten " ; expr buf e ; oc buf ')'
  | Eprim (Filter e)-> o buf "(filter " ; expr buf e ; oc buf ')'
  | Eprim (Drop e) -> o buf "(drop " ; expr buf e ; oc buf ')'
  | Eprim (Head e) -> o buf "(head " ; expr buf e ; oc buf ')'
  | Enull          -> o buf "null"
  | When  (e1, e2)     -> binop buf e1 " when " e2
  | Arrow (e1, e2)     -> binop buf e1 " -> " e2
  | Semi  (e1, e2)     -> binop buf e1 " ; " e2
  | Binop (op, e1, e2) -> binop buf e1 (bop op) e2 
  | Earray  el         -> array buf expr el
  | Etuple  el         -> tuple buf expr el
  | Evariant (s, e)    ->
      o buf "<\"" ; 
      o buf s ; o buf "\":" ; expr buf e ; 
      o buf ">"
  | Eobject fl         -> record buf expr_field fl

and expr_field buf = function
  | Fname s -> o buf s
  | Field (b, s, e) -> 
      if b then oc buf '?' ; 
      string buf s ; oc buf ':' ; expr buf e

and binop buf e1 op e2 =
  oc buf '(' ; expr buf e1 ; 
  o buf op ;
  expr buf e2 ; oc buf ')'				   

let buf = Buffer.create 1048
let print oc f e = Buffer.clear buf ; f buf e ; Buffer.output_buffer oc buf
let print_expr oc e = print oc expr e
let print_value oc e = print oc value e
let sov x = Buffer.clear buf ; value buf x ; Buffer.contents buf
let soe x = Buffer.clear buf ; expr buf x ; Buffer.contents buf    

let print_nnull_value oc = function 
  | Null | Pfailed -> () 
  | e -> print oc value e ; print_newline()
