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

exception Pat_failure

let rec filter t p e = 
  match p with
 | Any -> t | Id x -> SMap.add x e t
 | Type p -> 
     let ty = get_type e in
     if ty = Tany || ty = p then t else raise Pat_failure
 | When (p, b) -> 
     let t = filter t p e in
     (match expr t b with Bool true -> t | _ -> raise Pat_failure)
 | Binop (As, p, Id x) -> SMap.add x e (filter t p e)
 | Binop (Bar, p1, p2) -> (try filter t p1 e with Pat_failure -> filter t p2 e)
 | Binop (Apply, (Val (String _) as s), Earray l) -> filter t (Earray (s :: l)) e
 | Binop (Apply, (Val (String _) as s), x) -> filter t (Earray [s; x]) e
 | _ -> filter2 t p e

and filter2 t p e = 
  match p, e with
  | Val (String s1), String s2 when smatch s1 s2 -> t
  | Val v1, v2 when v1 = v2 -> t
  | Etuple [], Tuple [] -> t
  | Etuple (x1 :: rl1), Tuple (x2 :: rl2) -> 
      filter2 (filter t x1 x2) (Etuple rl1) (Tuple rl2)
  | Evariant (x1, v1), Variant (x2, v2) when x1 = x2 -> filter t v1 v2
  | Earray el1, Array el2 -> filter_list t el1 el2 
  | Eobject fdl, Object fd_map -> filter_fields fdl fd_map t
  | Binop (Cons, x, e), Array (y :: v) -> filter (filter t x y) e (Array v)
  | _ -> raise Pat_failure

and filter_list t el1 el2 = 
  match el1, el2 with
  | [], [] -> t
  | [], _ | _, [] -> raise Pat_failure
  | p :: pl, e :: el -> filter_list (filter t p e) pl el

and filter_field t pat opt obj fd = 
  try filter t pat (SMap.find fd obj)
  with 
  | Not_found when pat = Val Null -> t
  | Not_found when opt -> t
  | Not_found -> raise Pat_failure

and filter_fields p obj t = 
  match p with
  | [] -> t 
  | Fname n :: _ -> SMap.add n (Object obj) t
  | Field (opt,fd, p) :: rl -> 
    let t = filter_field t p opt obj fd in
    filter_fields rl (SMap.remove fd obj) t

and expr t = function
  | Type _ | Any -> Null
  | Val (Prim p) -> Prim (prim t p)
  | Val v -> v
  | Id s -> (try SMap.find s t with Not_found -> Null)
  | When  (e1, e2) when expr t e2 = Bool true -> expr t e1
  | When  _ -> Pfailed
  | Arrow (e1, e2) -> Closure (arrow t e1 e2)
  | Semi (Binop (Def, e1, e2), e3) -> 
      expr t (Binop (Apply, Arrow (e1, e3), e2))
  | Semi (_, e2) -> expr t e2
  | Binop (op, e1, e2) -> binop t op (expr t e1) (expr t e2)
  | Earray  l -> Array  (List.map (expr t) l)
  | Etuple l -> Array (List.map (expr t) l)
  | Evariant (s,e) -> Variant (s, expr t e)
  | Eobject l -> Object (List.fold_left (field t) SMap.empty l)

and field t acc fd =
  match fd with
  | Field (_, s, e) -> SMap.add s (expr t e) acc
  | Fname x -> 
      try match SMap.find x t with 
      | Object fds -> SMap.fold SMap.add fds acc
      | _ -> acc
      with Not_found -> acc

and prim t = function
  | Group 
  | Flatten as x -> x
  | Fold e -> Fold (Val (expr t e))
  | Filter e -> Filter (Val (expr t e))
  | Drop e -> Drop (Val (expr t e))
  | Head e -> Head (Val (expr t e))

and binop t op x y = 
  match op, x, y with
  | As , x, _ -> x
  | Seq, x, y -> seq t x y
  | Cons, v, Array l -> Array (v :: l)
  | Apply, Closure f, v -> f v
  | Apply, v, Array l -> Array (v :: l)
  | Apply, v, x -> Array [v; x]
  | Eq   , _, _ -> Bool (JsonAst.compare x y = 0)
  | Lt   , _, _ -> Bool (JsonAst.compare x y < 0)
  | Gt   , _, _ -> Bool (JsonAst.compare x y > 0)
  | Lte  , _, _ -> Bool (JsonAst.compare x y <= 0)
  | Gte  , _, _ -> Bool (JsonAst.compare x y >= 0)
  | Diff , _, _ -> Bool (JsonAst.compare x y <> 0)
  | And, Bool true, Bool true -> Bool true
  | And, _, _ -> Bool false
  | Or, Bool true, _ | Or, _, Bool true -> Bool true
  | Or, _, _ -> Bool false
  | Plus , Int x, Int y -> Int (Big_int.add_big_int x y)
  | Minus, Int x, Int y -> Int (Big_int.sub_big_int x y)
  | Mult , Int x, Int y -> Int (Big_int.mult_big_int x y)
  | Div  , Int x, Int y -> Int (Big_int.div_big_int x y)
  | Plus , Float x, Float y -> Float (x +. y)
  | Minus, Float x, Float y -> Float (x -. y)
  | Mult , Float x, Float y -> Float (x *. y)
  | Div  , Float x, Float y -> Float (x /. y)
  | _, (Float _ as x), Int y -> 
      binop t op x (Float (Big_int.float_of_big_int y))
  | _, Int x, (Float _ as y) -> 
      binop t op (Float (Big_int.float_of_big_int x)) y
  | Plus, String s1, String s2 -> String (s1^s2)
  | Plus, String _, y -> binop t op x (String (AstPp.sov y))
  | Plus, x, String _ -> binop t op (String (AstPp.sov x)) y
  | Plus, Object o1, Object o2 -> Object (SMap.fold SMap.add o2 o1)
  | Plus, Array l1, Array l2 -> Array (l1 @ l2)
  | Dot, Object o, String s -> (try SMap.find s o with Not_found -> Null)
  | Dot, Array y, Int x  -> List.nth y (Big_int.int_of_big_int x)
  | Bar, Closure f1, Closure f2 -> 
      Closure (fun x -> 
	match f1 x with Pfailed -> f2 x | x -> x)
  | Bar, Bool false, x 
  | Bar, Pfailed, x -> x
  | _ -> failwith (AstPp.soe (Binop(op,Val x,Val y)))

and arrow t e1 e2 = fun x ->
  let t, b = 
    try  filter t e1 x, true 
    with Pat_failure -> t, false in
  if b then expr t e2 else Pfailed

and seq t x y = 
  match x, y with
  | Flow x, Closure f -> Flow (Flow.map f x)
  | Flow x, Prim Group -> Flow (group x)
  | Flow x, Prim Flatten -> Flow (flatten x)
  | Flow x, Prim (Fold (Val e)) -> Flow (fold x e)
  | Flow x, Prim (Filter (Val f)) -> Flow (sfilter x f)
  | Flow x, Prim (Drop (Val (Int n))) -> Flow (Flow.drop n x)
  | Flow x, Prim (Head (Val (Int n))) -> Flow (Flow.head n x)
  | _ -> failwith (AstPp.soe (Binop(Seq, Val x, Val y)))

and sfilter x = function
  | Closure f -> 
      Flow.filter (fun x -> match f x with Bool b -> b | _ -> false) x
  | _ -> x

and fold x = function
  | Array [Closure f1 ; Closure f2 ; acc] ->
      let f1 = fun x acc -> 
	match f1 (Array [x;acc]) with 
	| Array [x;acc] -> x, acc 
	| _ -> x, acc in
      Flow.folds f1 f2 acc x
  | _ -> x

and group x = 
  x ++ Flow.filter (function Array [x ; y] -> true | _ -> false)
    ++ Flow.map (function Array [x ; y] -> (x,y) | _ -> assert false)
    ++ Flow.group
    ++ Flow.map (fun (x,y) -> Array [x ; Array y])

and flatten x =
  x ++ Flow.map (function Array l -> l | x -> [x])
    ++ Flow.flatten

let program prog = expr !env prog
