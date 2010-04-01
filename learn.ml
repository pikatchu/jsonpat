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

open JsonAst
open Util

module VMap = Map.Make (JsonAst)

let gen_var = let i = ref 0 in fun () -> incr i ; "x"^(string_of_int !i)

let rec load_value t v = 
  match v with
  | Null -> t
  | Prim _
  | Pfailed | Flow _ | Closure _ -> assert false
  | Bool _ | Float _ | Int _
  | String _ as v -> VMap.add v (Id (gen_var())) t
  | Array l -> List.fold_left load_value t l
  | Object m -> List.fold_left (fun t (_,x) -> load_value t x) t (elements m)

let rec expr get nf = function
  | Null -> Val Null
  | Flow _ | Closure _ | Prim _
  | Pfailed -> assert false
  | Bool _ | Float _ | Int _
  | String _ as x -> 
      (try get x
      with Not_found -> nf x)
  | Array l -> 
      let l = List.map (expr get nf) l in
      if List.filter (function Any -> false | _ -> true) l = []
      then Any
      else Earray l
  | Object m -> 
      let l = List.fold_right (field get nf) (elements m) [] in
      if l = []
      then Any
      else Eobject l

and field get nf (x,y) acc = 
  match expr get nf y with
  | Any -> acc 
  | y -> Field (true, x, y) :: acc

let get_pat t x = 
  let res = VMap.find x !t in 
  t := VMap.remove x !t ; 
  res

let nf_pat _ = Any
let get_expr t x = VMap.find x t
let nf_expr x = Val x

let prog genv v = 
  let lb = Lexing.from_string genv.Genv.learn in
  let res = Lexer.value lb in
  let t = load_value VMap.empty res in
  let pat = expr (get_pat (ref t)) nf_pat v in
  let body = expr (get_expr t) nf_expr res in
  Arrow (pat, body)
