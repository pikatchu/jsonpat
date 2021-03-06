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


module CheckEq: sig

  val expr: expr -> unit
end = struct
  let msg = "Error: unexpected \"=\" did you mean \"==\" ?\n"
  let error () = output_string stderr msg ; exit 1

  let rec expr = function
    | Eflow _|Estring _|Eint _|Efloat _|Ebool _|Enull
    | Id _ | Type _ | Any -> ()
    | Eprim p -> prim p
    | Binop (Def, e1, e2) -> error ()
    | When (e1, e2)
    | Arrow (e1, e2) 
    | Binop (_, e1, e2) 
    | Semi (Binop (Def, _, e1), e2) -> expr e1 ; expr e2
    | Evariant (_, e)
    | Semi (_, e) -> expr e
    | Etuple el
    | Earray el -> List.iter expr el 
    | Eobject fdl -> List.iter field fdl 

  and field = function
    | Fname _ -> ()
    | Field (_,_,e) -> expr e

  and prim = function
  | Group
  | Flatten -> ()
  | Fold e
  | Filter e
  | Drop e
  | Head e -> expr e

end

let error msg e = 
  Printf.fprintf stderr "Error in expression \"" ;
  JsonpatAstPp.print_expr stderr e ;
  Printf.fprintf stderr "\", %s\n" msg ;
  exit 1

let rec pattern t = function
  | Id x -> SSet.add x t
  | Estring _|Eint _|Efloat _|Ebool _|Enull
  | Any | Type _ -> t
  | When (p, b) -> let t = pattern t p in expr t b ; t
  | Binop ((As | Bar | Cons), p1, p2) ->  pattern (pattern t p1) p2
  | Evariant (_, p)
  | Binop (Apply, Estring _, p) -> pattern t p
  | Eflow _ | Eprim _ 
  | Arrow _ | Semi _ | Binop _ as e -> error "invalid pattern" e 
  | Etuple pl 
  | Earray pl -> List.fold_left pattern t pl 
  | Eobject fl -> List.fold_left pfield t fl 

and pfield t = function
  | Fname x -> SSet.add x t 
  | Field (_, _, e) -> pattern t e

and arrow t = function
  | Id _
  | Arrow _ 
  | Eprim _
  | Binop ((Dot | Apply), _, _) -> ()
  | Binop (Seq, a1, a2) -> arrow t a1 ; arrow t a2
  | Binop (Bar, a1, a2) -> arrow t a1 ; arrow t a2
  | Semi (_, a) -> arrow t a
  | e -> error "was expecting a function" e

and expr t = function
  | Eflow _|Estring _|Eint _|Efloat _|Ebool _|Enull -> ()
  | Eprim p -> prim t p
  | Id x when SSet.mem x t -> ()
  | Id _ as e -> error "unbound name" e
  | When (e1, e2) -> expr t e1 ; expr t e2 
  | Arrow (p, e) -> expr (pattern t p) e 
  | Semi (Binop (Def, e1, e2), e3) -> expr t e2 ; expr (pattern t e1) e3
  | Binop (Seq, e1, e2) -> arrow t e1 ; arrow t e2 ; expr t e1 ; expr t e2
  | Evariant (_, e)
  | Binop (Apply, Estring _, e) -> expr t e
  | Binop (Apply, e1, e2) -> arrow t e1 ; expr t e1 ; expr t e2
  | Binop (_, e1, e2) -> expr t e1 ; expr t e2
  | Etuple el
  | Earray el -> List.iter (expr t) el
  | Eobject fdl -> List.iter (field t) fdl 
  | Type _ | Any as e -> error "wasn't expecting a pattern" e
  | Semi _ as e -> error "bad usage of ;" e

and prim t = function
  | Group
  | Flatten -> ()
  | Fold e
  | Filter e
  | Drop e
  | Head e -> expr t e

and field t = function
  | Fname _ -> ()
  | Field (_, _, e) -> expr t e 


let program p = 
  let env = SMap.fold (fun x _ acc -> SSet.add x acc) !env SSet.empty in
  CheckEq.expr p ; expr env p
