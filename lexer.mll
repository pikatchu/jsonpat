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
(* Jsonpat Lexer                                                             *)
(* Used for tokens of the language (token entry)                             *)
(* Used to parse json values directly (value entry)                          *)
(*****************************************************************************)

{
open Lexing
open Parser
open Pos
open JsonAst
open Util


(* Buffer used by the "string" lexer to accumulate chars *)
(* One can't directly output the current lexeme because some characters *)
(* are escaped *)

let sbuf = Buffer.create 256

let keyword_l = [ 
  "null" , NULL   ; "as"    , AS      ; "true"  , TRUE    ;
  "false", FALSE  ; "when"  , WHEN    ; "int"   , TINT    ;
  "bool" , TBOOL  ; "float" , TFLOAT  ; "string", TSTRING ;
  "array", TARRAY ; "object", TOBJECT ; "group" , GROUP ;
  "flatten", FLATTEN ; "fold", FOLD   ; "filter", FILTER ;
  "drop" , DROP ;
]

let fill_table ht l = List.iter (fun (x,y) -> Hashtbl.add ht x y) l ; ht
let keyword_t = fill_table (Hashtbl.create 23) keyword_l

let ident x = 
  try Hashtbl.find keyword_t x 
  with Not_found -> ID x

let int lexbuf = Int (ios (lexeme lexbuf))
let float lexbuf = Float (fos (lexeme lexbuf))
let add_field fd acc = function JsonAst.Null -> acc | v -> SMap.add fd v acc

let cstr = function 
  | "Infinity" -> FLOAT "inf" 
  | "NaN" -> FLOAT "nan" 
  | x -> STRING x

(*****************************************************************************)
}

let eol      = '\n'
let digit    = ['0'-'9']
let nonzero  = ['1'-'9']
let upper    = ['A'-'Z']
let lower    = ['a'-'z']
let space    = '\t' | ' ' | '\r'
let esc_self = '\\' | '"' | '/'
let e        = ['e' 'E']['+' '-']?
let digits   = digit+
let exp      = e digits
let frac     = '.' digits
let lcomment = "//"[^'\n']*
let char     = ['\x20'-'\x21' '\x23'-'\x5B' '\x5D'-'\xFF' ]
let hex      = [ '0'-'9' 'a'-'f' 'A'-'F' ]
let int      = '-'? (digit | nonzero digits)
let float    = int frac | int exp | int frac exp
let alpha    = (upper | lower | '_')
let id       = lower (alpha | digit)*
let cstr     = upper (alpha | digit)*
let hex4     = hex hex hex hex

rule token = parse 
| space             { token lexbuf }
| eof               { EOF }
| '{'               { LCB }
| '<'               { LT }
| "<="              { LTE }
| '>'               { GT }
| ">="              { GTE }
| ':'               { COL }
| ';'               { SEMI }
| "::"              { COLCOL }
| '}'               { RCB }
| ','               { COMMA }
| '['               { LB }
| ']'               { RB }
| "->"              { ARROW }
| '.'               { DOT }
| '|'               { BAR }
| '('               { LP }
| ')'               { RP }
| '<'               { LT }
| '>'               { GT }
| ">>"              { GTGT }
| "="               { EQ }
| "=="              { EQEQ }
| "<>"              { NEQ }
| '_'               { UNDERSCORE } 
| '+'               { PLUS }
| '-'               { MINUS }
| '*'               { MULT }
| '/'               { DIV }
| "&&"              { AMPAMP }
| "||"              { BARBAR }
| '?'               { QMARK }
| '"'               { Buffer.clear sbuf ; STRING (string lexbuf) }
| "/*"              { comment lexbuf ; token lexbuf }
| lcomment          { token lexbuf }
| eol               { newline lexbuf ; token lexbuf }
| int               { INT   (lexeme lexbuf) }
| float             { FLOAT (lexeme lexbuf) }
| id                { ident (lexeme lexbuf) } 
| cstr              { cstr  (lexeme lexbuf) }
| _                 { syntax_error lexbuf }

and string = parse
| '\"'              { Buffer.contents sbuf }
| '\\'              { escape lexbuf }
| char+             { o sbuf (lexeme lexbuf) ; string lexbuf }
| _                 { syntax_error lexbuf }

and escape = parse
| 'u' (hex4 as x)   { o sbuf (utf8_val x) ; string lexbuf }
| 'b'               { oc sbuf '\b' ; string lexbuf }
| 'f'               { oc sbuf '\012' ; string lexbuf }
| 't'               { oc sbuf '\t' ; string lexbuf }
| 'n'               { oc sbuf '\n' ; string lexbuf }
| 'r'               { oc sbuf '\r' ; string lexbuf }
| (esc_self as c)   { oc sbuf c ; string lexbuf }
| _                 { syntax_error lexbuf }

and comment = parse
| "*/"              { () }
| eof               { syntax_error lexbuf }
| eol               { newline lexbuf ; comment lexbuf }
| _                 { comment lexbuf }

(*****************************************************************************)
(* Function parsing a json value                                             *)
(* As much as I dislike this approach (as opposed to using a parser)         *)
(* The produced code is much more efficient (~ 40% gain)                     *)
(*****************************************************************************)

and value = parse 
| eof               { raise End_of_file }
| "/*"              { comment lexbuf ; value lexbuf }
| eol               { newline lexbuf ; value lexbuf }
| space+            { value lexbuf }
| "null"            { Null }
| "true"            { Bool true }
| "false"           { Bool false }
| "Infinity"        { Float infinity }
| "-Infinity"       { Float neg_infinity }
| "NaN"             { Float nan }
| int               { int lexbuf }
| float             { float lexbuf }
| '"'               { Buffer.clear sbuf ; String (string lexbuf) }
| '['               { Array (array lexbuf) }
| '{'               { Object (object_ SMap.empty lexbuf) }
| _                 { syntax_error lexbuf }


(* Arh ... this is ugly *)
and array = parse
| "/*"              { comment lexbuf ; array lexbuf }
| space+            { array lexbuf }
| eol               { newline lexbuf ; array lexbuf }
| "null"            { Null :: array_rl lexbuf }
| "true"            { Bool true :: array_rl lexbuf }
| "false"           { Bool false :: array_rl lexbuf }
| "Infinity"        { Float infinity :: array_rl lexbuf }
| "-Infinity"       { Float neg_infinity :: array_rl lexbuf }
| "NaN"             { Float nan :: array_rl lexbuf }
| int               { let v = int lexbuf in v :: array_rl lexbuf }
| float             { let v = float lexbuf in v :: array_rl lexbuf }
| '"'               { Buffer.clear sbuf ; 
		      let v = String (string lexbuf) in 
		      v :: array_rl lexbuf }
| '['               { let v = array lexbuf in Array v :: array_rl lexbuf }
| ']'               { [] }
| '{'               { let v = object_ SMap.empty lexbuf in 
                      Object v :: array_rl lexbuf }
| _                 { syntax_error lexbuf }

and array_rl = parse
| "/*"              { comment lexbuf ; array_rl lexbuf }
| space+            { array_rl lexbuf }
| eol               { newline lexbuf ; array_rl lexbuf }
| ']'               { [] }
| ','               { array lexbuf }
| _                 { syntax_error lexbuf }

and object_ acc = parse
| "/*"              { comment lexbuf ; object_ acc lexbuf }
| space+            { object_ acc lexbuf }
| eol               { newline lexbuf ; object_ acc lexbuf }
| '}'               { acc }
| '"'               { Buffer.clear sbuf ; field (string lexbuf) acc lexbuf }
| _                 { syntax_error lexbuf }

and object_rl acc = parse
| "/*"              { comment lexbuf ; object_rl acc lexbuf }
| space+            { object_rl acc lexbuf }
| eol               { newline lexbuf ; object_rl acc lexbuf }
| ','               { object_ acc lexbuf }
| '}'               { acc }
| _                 { syntax_error lexbuf }

and field fd acc = parse
| "/*"              { comment lexbuf ; field fd acc lexbuf }
| space+            { field fd acc lexbuf }
| eol               { newline lexbuf ; field fd acc lexbuf }
| ':'               { object_rl (add_field fd acc (value lexbuf)) lexbuf }
| _                 { syntax_error lexbuf }
