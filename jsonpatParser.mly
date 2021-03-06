%{ 
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
open JsonpatAst
open JsonpatUtil

let tuple = function
  | [x] -> x
  | _ as x -> Etuple x

let vobject l =
  List.fold_left 
    (fun acc (x,y) -> SMap.add x y acc) 
    SMap.empty 
    l

%}

%token NULL UNDERSCORE EOF AS
%token TRUE FALSE COLCOL EQEQ
%token <string> INT FLOAT STRING ID 
%token TINT TBOOL TFLOAT TSTRING TOBJECT TARRAY
%token LCB COL RCB COMMA LB RB SEMI
%token ARROW DOT WHEN BAR LP RP AMPAMP BARBAR
%token LT GT GTGT EQ LT GT LTE GTE NEQ
%token IN PLUS MINUS MULT DIV QMARK
%token GROUP FLATTEN FOLD FILTER DROP HEAD

%right SEMI
%left GTGT
%nonassoc EQ
%right BAR
%right ARROW
%left WHEN
%left COMMA
%left AS
%left AMPAMP
%left BARBAR
%left EQEQ NEQ LT GT LTE GTE
%right COLCOL
%left PLUS MINUS
%left MULT DIV
%nonassoc umin

%start expr value
%type <JsonpatAst.expr> expr
%type <JsonpatAst.value> value
%%

value:
| NULL			{ Null }
| TRUE			{ Bool true }
| FALSE			{ Bool false }
| INT			{ Int (Big_int.big_int_of_string $1) }
| FLOAT			{ Float (fos $1) }
| STRING		{ String $1 }
| LB value_l RB		{ Array $2 }
| LCB vfield_l RB	{ Object (vobject  $2) }

value_l:
|			{ [] }
| value			{ [$1] }
| value COMMA value_l	{ $1 :: $3 }

vfield_l:
|			{ [] }
| STRING COL value	{ [$1,$3] }
| STRING COL value
    COMMA vfield_l	{ ($1, $3) :: $5 }

simpl:
| UNDERSCORE            { Any }
| TINT                  { Type Tint }
| TBOOL                 { Type Tbool }
| TFLOAT                { Type Tfloat }
| TSTRING               { Type Tstring }
| TOBJECT               { Type Tobject }
| TARRAY                { Type Tarray }
| NULL                  { Enull }
| LP RP                 { Enull }
| TRUE                  { Ebool true }
| FALSE                 { Ebool false }
| INT                   { Eint (Big_int.big_int_of_string $1) }
| FLOAT                 { Efloat (fos $1) }
| STRING                { Estring $1 }
| ID                    { Id $1 }
| GROUP                 { Eprim Group }
| FLATTEN               { Eprim Flatten }
| simpl DOT ID          { Binop (Dot, $1, Estring $3) }
| simpl DOT STRING      { Binop (Dot, $1, Estring $3) }
| simpl DOT LB INT RB   { Binop (Dot, $1, 
				 Eint (Big_int.big_int_of_string $4)) }
| LB  expr_l  RB        { Earray $2 }
| LCB field_l RCB       { Eobject $2 }
| LP expr_t RP          { tuple $2 }

expr_t:
| expr                  { [$1] }
| expr COMMA expr_t     { $1 :: $3 }

expr:
| simpl                 { $1 }
| simpl simpl           { Binop (Apply, $1, $2) }
| expr AS expr          { Binop (As, $1, $3) }
| expr BAR expr         { Binop (Bar, $1, $3) }
| expr ARROW expr       { Arrow ($1, $3) }
| expr GTGT expr        { Binop (Seq, $1, $3) }
| expr EQ expr          { Binop (Def, $1, $3) }
| expr EQEQ expr        { Binop (Eq, $1, $3) }
| expr LT expr          { Binop (Lt, $1, $3) } 
| expr GT expr          { Binop (Gt, $1, $3) } 
| expr LTE expr         { Binop (Lte, $1, $3) } 
| expr GTE expr         { Binop (Gte, $1, $3) } 
| expr NEQ expr         { Binop (Diff, $1, $3) }
| expr PLUS expr        { Binop (Plus, $1, $3) } 
| expr MINUS expr       { Binop (Minus, $1, $3) }
| expr MULT expr        { Binop (Mult, $1, $3) } 
| expr DIV expr         { Binop (Div, $1, $3) } 
| expr COLCOL expr      { Binop (Cons, $1, $3) }
| expr BARBAR expr      { Binop (Or, $1, $3) }
| expr AMPAMP expr      { Binop (And, $1, $3) }
| expr WHEN expr        { When ($1, $3) }
| expr SEMI expr        { Semi ($1, $3) }
| FOLD simpl            { Eprim (Fold $2) }
| FILTER simpl          { Eprim (Filter $2) }
| DROP simpl            { Eprim (Drop $2) }
| HEAD simpl            { Eprim (Head $2) }
| MINUS expr %prec umin { Binop (Minus, Eint Big_int.zero_big_int, $2) }
| LT STRING GT          { Evariant ($2, Enull) }
| LT STRING COL expr GT { Evariant ($2, $4) }

field_l: 
|                       { [] }
| field                 { [$1] }
| field COMMA field_l   { $1 :: $3 }

field:
| ID                    { Fname $1 }
| STRING COL expr       { Field (false, $1, $3) }
| ID COL expr           { Field (false, $1, $3) }
| QMARK STRING COL expr { Field (true, $2, $4) }
| QMARK ID COL expr     { Field (true, $2, $4) }

expr_l: 
|                       { [] }
| expr                  { [$1] }
| expr COMMA expr_l     { $1 :: $3 }

