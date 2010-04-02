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

open Util
open JsonAst

let threshold = ref 0

type t = ty list
and  ty = 
  | Tobject of (string * t) list
  | Tarray of t
  | Tint of int * int
  | Tstring
  | Tcstr of SSet.t
  | Tfloat 
  | Tbool
  | Tnull
  | Tid of string

(* Please note that Tcstr must have the same order as Tstring                *)
(* That's because we want them to be unified                                 *)
let order = function
  | Tobject _ -> 0 
  | Tarray _  -> 1 
  | Tint _    -> 2
  | Tcstr _   -> 3 
  | Tstring   -> 3
  | Tfloat    -> 4
  | Tbool     -> 5
  | Tnull     -> 6
  | Tid _     -> assert false

let compare x y = order x - order y

let rec unify l1 l2 = 
  match l1, l2 with
  | [], l | l, [] -> l
  | x1 :: rl1, x2 :: rl2 -> 
      let n = compare x1 x2 in
      if n < 0
      then x1 :: unify rl1 l2
      else if n > 0
      then x2 :: unify l1 rl2
      else unify_t x1 x2 (unify rl1 rl2)

and unify_t x y acc = 
  match x, y with
  | Tnull, Tnull -> Tnull :: acc
  | Tobject fds1, Tobject fds2 -> Tobject (unify_fields fds1 fds2) :: acc
  | Tstring, Tstring -> Tstring :: acc
  | Tint (x1, x2), Tint (y1, y2) -> Tint (min x1 y1, max x2 y2) :: acc
  | Tfloat, Tfloat -> Tfloat :: acc
  | Tbool, Tbool -> Tbool :: acc
  | Tcstr s1, Tcstr s2 -> 
      let s = SSet.union s1 s2 in
      if SSet.cardinal s > !threshold 
      then Tstring :: acc
      else Tcstr s :: acc
  | Tarray t1, Tarray t2 -> Tarray (unify t1 t2) :: acc
  | Tcstr s, Tstring
  | Tstring , Tcstr s -> Tstring :: acc
  | x, y when compare x y < 0 -> x :: y :: acc
  | x, y when compare x y > 0 -> y :: x :: acc
  | _ -> assert false

and unify_fields l1 l2 = 
  match l1, l2 with
  | l, [] | [], l -> List.fold_right option_field l []
  | (x1, t1) :: rl1, (x2, _) :: _ when String.compare x1 x2 < 0 ->
      option_field (x1, t1) (unify_fields rl1 l2)
  | (x1, _) :: _, (x2, t2) :: rl2 when String.compare x1 x2 > 0 -> 
      option_field (x2, t2) (unify_fields l1 rl2)
  | (x1, t1) :: rl1, (_, t2) :: rl2 -> 
      (x1, unify t1 t2) :: (unify_fields rl1 rl2)

and option_field (s, l) acc = (s, unify [Tnull] l) :: acc

let rec type_value = function
  | Object l -> [Tobject (type_object l)]
  | Array l  -> [Tarray (type_array l)]
  | String s -> [Tcstr (SSet.singleton s)]
  | Int n    -> [Tint (n, n)]
  | Float _  -> [Tfloat]
  | Bool _   -> [Tbool]
  | _        -> [Tnull]

and type_object l = 
  List.sort (fun (x,_) (y,_) -> String.compare x y)
    (List.map (fun (x,y) -> x, type_value y) (elements l))

and type_array l = List.fold_left (fun acc x -> unify (type_value x) acc) [] l

module Type = struct

  type t = ty

  let rec compare t1 t2 = 
    match t1, t2 with
    | Tobject t1, Tobject t2 -> compare_fields t1 t2
    | Tarray t1 , Tarray t2  -> compare_list t1 t2
    | Tint _    , Tint _ 
    | Tstring   , Tstring
    | Tcstr _   , Tstring
    | Tstring   , Tcstr _
    | Tfloat    , Tfloat
    | Tbool     , Tbool
    | Tnull     , _
    | _         , Tnull -> 0
    | x, y -> Pervasives.compare x y

  and compare_fields t1 t2 = 
    match t1, t2 with
    | [], _
    | _, [] -> 0
    | (s1,x1) :: rl1, (s2, x2) :: rl2 ->
	let c = String.compare s1 s2 in
	if c = 0
	then let c = compare_list x1 x2 in
	if c = 0
	then compare_fields rl1 rl2
	else c
	else c

  and compare_list l1 l2 = 
    match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x1 :: rl1, x2 :: rl2 ->
	let c = compare x1 x2 in
	if c = 0 then compare_list rl1 rl2
	else c

end

module TMap = Map.Make (Type)

module Pp: sig
  val print: (string -> unit) -> t -> unit
end = struct

  let soi = string_of_int
  let string x = "\""^x^"\""

  let find x (t1, t2, t3, acc, rt1) = 
    let name = TMap.find x t1 in
    let rt1 = SMap.add name (unify (SMap.find name rt1) [x]) rt1 in
    (t1,t2,t3,acc,rt1), name

  let add ty x (t1, t2, t3, acc, rt1) = 
    let x = if SSet.mem x t3 then "t" else x in
    let name_n = try SMap.find x t2 with Not_found -> 0 in
    let name_n = name_n + 1 in
    let t2 = SMap.add x name_n t2 in
    let name = if name_n = 1 then x else x^(soi name_n) in
    let rt1 = SMap.add name [ty] rt1 in
    (TMap.add ty name t1, t2, t3, name :: acc, rt1), name

  let keywl = ["bool" ; "float" ; "type" ; "int" ; "array" ; "main" ; "string"]
  let keyws = List.fold_right SSet.add keywl SSet.empty
  let empty = TMap.empty, SMap.empty, keyws, [], SMap.empty

  let rec make_t name l t = List.fold_right (make_acc name) l (t, [])
  and make_ty name t = function
  | Tint _ | Tstring 
  | Tfloat | Tbool   | Tnull
  | Tid _ as x -> t, x
  | Tobject fdl as x -> 
      (try 
	let t, name = find x t in
	t, Tid name
      with Not_found ->
	let t, fdl = List.fold_right field fdl (t, []) in 
	let ty = Tobject fdl in
	let t, name = add ty name t in
	t, Tid name)
  | Tarray l -> 
      let t, l = make_t name l t in 
      t, Tarray l
  | Tcstr _ as x -> 
      try 
	let t, name = find x t in
	t, Tid name
      with Not_found -> 
	let t, name = add x name t in
	t, Tid name

  and make_acc name x (t, acc) = let t, x = make_ty name t x in t, x :: acc
  and field (s, x) (t, acc) = 
    let t, x = make_t (s^"_t") x t in 
    t, (s, x) :: acc

  let rec print_t o = function
    | [] -> ()
    | [x] -> print_ty o x 
    | x :: rl -> print_ty o x ; o " | " ; print_t o rl

  and print_ty o = function
    | Tobject fdl -> o "{\n" ; List.iter (print_field o) fdl ; o "}"
    | Tarray l -> o "(" ; print_t o l ; o ")" ; o " array"
    | Tint (n1, n2) -> o "int[" ; o (soi n1) ; o "," ; o (soi n2) ; o "]"
    | Tstring -> o "string"
    | Tcstr s -> o "\n  " ; print_set o 1 (SSet.elements s)
    | Tfloat -> o "float"
    | Tbool -> o "bool"
    | Tnull -> o "null"
    | Tid s -> o s

  and print_set o n = function
    | [] -> ()
    | [x] -> o (string x)
    | x :: rl -> 
	o (string x) ; 
	o " | " ; if n mod 4 = 0 then o "\n  " ;
	print_set o (n+1) rl

  and print_field o (s, x) = o "  " ; o s ; o ": " ; print_t o x ; o " ;\n"

  let print_def o env n = 
    let ty = SMap.find n env in
    o "and " ; o n ; o " = " ; print_t o ty ; o "\n\n"

  let print o t =
    let t, ty = make_t "main" t empty in
    let _,_,_,defl, env = t in
    o "type main = " ; print_t o ty ; o "\n\n" ;
    List.iter (print_def o env) defl
end

let show_type s = 
  let ty = Flow.fold (fun acc x -> unify acc (type_value x)) [] s in
  Pp.print print_string ty 
