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

open JsonpatUtil

(* Operators *)
type bop = 
  | Plus | Minus | Mult | Div       (* Arithmetics *)
  | Eq | Diff | Lt | Gt | Lte | Gte (* Comparisons *)
  | Seq | As | Bar | Dot | Cons 
  | Apply | Def | And | Or

type type_ = 
  | Tint | Tbool | Tfloat 
  | Tstring | Tobject | Tarray | Tany

type 'a prim =
  | Group
  | Flatten
  | Fold of 'a
  | Filter of 'a
  | Drop of 'a
  | Head of 'a

type flow = value JsonpatFlow.t

and value = 
  | Null
  | Pfailed
  | Bool of bool
  | Float of float
  | Int of Big_int.big_int
  | String of string
  | Prim of value prim
  | Closure of (value -> value)
  | Flow of flow
  | Array of value list
  | Tuple of value list
  | Variant of string * value
  | Object of value SMap.t

and expr =
  | Any
  | Enull
  | Ebool of bool
  | Efloat of float
  | Eint of Big_int.big_int
  | Estring of string
  | Eprim of expr prim
  | Eflow of value JsonpatFlow.t
  | Type of type_
  | Id  of string
  | When of expr * expr
  | Arrow of expr * expr
  | Semi of expr * expr
  | Binop of bop * expr * expr
  | Earray of expr list
  | Etuple of expr list
  | Evariant of string * expr
  | Eobject of field list

and field = 
  | Fname of string
  | Field of bool * string * expr

type t = value

let get_type = function
  | Bool   _ -> Tbool 
  | Float  _ -> Tfloat 
  | Int    _ -> Tint
  | String _ -> Tstring
  | Array  _ -> Tarray 
  | Object _ -> Tobject
  | _ -> Tany

let rec add_left x = function
  | Binop (Seq, z, t) -> Binop (Seq, add_left x z, t)
  | Semi (eq, y) -> Semi (eq, add_left x y)
  | y -> Binop (Seq, x, y)

let rec compare_list f l1 l2 = 
  match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x1 :: rl1, x2 :: rl2 ->
      let c = f x1 x2 in
      if c = 0
      then compare_list f rl1 rl2
      else c

let rec compare v1 v2 = 
  match v1, v2 with
  | Array  x, Array y -> compare_list compare x y
  | Object x, Object y -> compare_list compare_fields (elements x) (elements y)
  | Int n1, Int n2 -> Big_int.compare_big_int n1 n2
  | Int _, _ -> -1
  | _, Int _ -> 1
  | x, y -> Pervasives.compare x y

and compare_fields (s1, v1) (s2, v2) =
  let c = String.compare s1 s2 in
  if c = 0
  then compare v1 v2
  else c

let env = ref SMap.empty
let register name f = env := SMap.add name (Closure f) !env
