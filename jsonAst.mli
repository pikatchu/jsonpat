(*
  Copyright (c) 2010, Julien Verlaguet
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

  3. Neither the name of Julien Verlaguet nor the names of
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

(* Operators *)
type bop = 
  | Plus | Minus | Mult | Div       (* Arithmetics *)
  | Eq | Diff | Lt | Gt | Lte | Gte (* Comparisons *)
  | Seq | As | Bar | Dot | Cons 
  | Apply | Def | And | Or

type type_ = 
  | Tint | Tbool | Tfloat 
  | Tstring | Tobject | Tarray | Tany

type flow = value Flow.t

and value = 
  | Null
  | Pfailed
  | Bool of bool
  | Float of float
  | Int of int
  | String of string
  | Prim of prim
  | Closure of (value -> value)
  | Flow of flow
  | Array of value list
  | Object of value Util.SMap.t

and prim =
  | Group
  | Flatten
  | Fold of expr
  | Filter of expr
  | Drop of expr

and expr =
  | Any
  | Val of value
  | Type of type_
  | Id  of string
  | When of expr * expr
  | Arrow of expr * expr
  | Semi of expr * expr
  | Binop of bop * expr * expr
  | Earray of expr list
  | Eobject of field list

and field = 
  | Fname of string
  | Field of bool * string * expr

type t = value

val get_type : value -> type_
val add_left : expr -> expr -> expr
val compare : value -> value -> int
val env : value Util.SMap.t ref
val register : string -> (value -> value) -> unit
