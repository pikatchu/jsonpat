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

module SMap = Map.Make (String)
module SSet = Set.Make (String)

let o = Buffer.add_string
let oc = Buffer.add_char

let (++) x f = f x

let hexval c =
  match c with
  | '0'..'9' -> int_of_char c - int_of_char '0'
  | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
  | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
  | _ -> assert false

let ios = int_of_string
let soi = string_of_int
let fos = float_of_string
let sof = string_of_float
let foi = float_of_int

let ioh x = 
  0x1000 * hexval x.[0] +
    0x100 * hexval x.[1] +
    0x10 * hexval x.[2] + 
    hexval x.[3]

let utf8_val x = Netconversion.ustring_of_uchar `Enc_utf8 (ioh x)

let process_file f fn = 
  try f (open_in fn) 
  with Sys_error s -> 
    Printf.fprintf stderr "Couldn't open file %s, %s\n" fn s ; 
    exit 1

let elements m = SMap.fold (fun x y acc -> (x,y) :: acc) m []

let smatch x y = 
  Str.string_match (Str.regexp x) y 0 && 
  Str.match_end() = String.length y
