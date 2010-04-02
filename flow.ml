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


type 'a t =
  | Empty
  | Cons of 'a * 'a t Lazy.t

let car = function
  | Empty -> failwith "car on empty flow"
  | Cons (x, _) -> x

let rec iter f = function
  | Empty -> ()
  | Cons (x, rl) -> f x ; iter f (Lazy.force rl)

let rec flatten = function
  | Empty -> Empty
  | Cons ([], rl) -> flatten (Lazy.force rl)
  | Cons (x :: y, rl) -> Cons (x, lazy (flatten (Cons (y, rl))))

let rec group last acc = function
  | Empty -> Cons ((last, acc), lazy Empty)
  | Cons ((x,y), rl) when x = last -> group x (y :: acc) (Lazy.force rl)
  | Cons ((x,y), rl) -> Cons ((last, acc), lazy (group x [y] (Lazy.force rl)))

let group = function
  | Empty -> Empty 
  | Cons ((x, y), rl) -> group x [y] (Lazy.force rl)

let rec filter f = function
  | Empty -> Empty
  | Cons (x, rl) when f x -> Cons (x, lazy (filter f (Lazy.force rl)))
  | Cons (_, rl) -> filter f (Lazy.force rl)

let rec map f = function
  | Empty -> Empty
  | Cons (x, y) -> Cons (f x, lazy (map f (Lazy.force y)))

let rec cat f ic = 
  let v = try Some (f ic) with End_of_file -> None in
  match v with
  | None -> Empty
  | Some x -> Cons (x, lazy (cat f ic))

let rec cat_k k f ic = 
  let v = try Some (f ic) with End_of_file -> None in
  match v with
  | None -> k ()
  | Some x -> Cons (x, lazy (cat_k k f ic))

let rec cat_files f1 f2 f3 = function
  | [] -> Empty
  | x :: rl ->
      f2 x ;
      let ic = open_in x in
      cat_k (fun () -> close_in ic ; cat_files f1 f2 f3 rl) f1 (f3 ic)
      
let rec folds f1 f2 acc = function
  | Empty -> Cons (f2 acc, lazy Empty)
  | Cons (x, y) -> 
      let x, acc = f1 x acc in
      Cons (x, lazy (folds f1 f2 acc (Lazy.force y)))

let rec fold f acc = function
  | Empty -> acc
  | Cons (x, y) -> fold f (f acc x) (Lazy.force y)

let rec filter f = function
  | Empty -> Empty
  | Cons (x, y) when f x -> Cons (x, lazy (filter f (Lazy.force y)))
  | Cons (x, y) -> filter f (Lazy.force y)

let rec drop n l =
  if n = 0 then l
  else match l with
  | Empty -> Empty
  | Cons (x, rl) -> drop (n-1) (Lazy.force rl)

