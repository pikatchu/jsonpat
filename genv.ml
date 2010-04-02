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

let version = "0.7.0"

let print_version () = 
  print_string version ; 
  print_newline() ;
  exit 0

type t = {
    prog: string ;
    prog_file: string ;
    inline: bool ;
    files: string list ;
    cmxs: string list ;
    learn: string ;
    print: bool ;
    show_type: bool ;
    threshold: int ;
    version: string ;
  }

let string r = Arg.String (fun x -> r := x)
let string_l r = Arg.String (fun x -> r := Str.split (Str.regexp "[ \t,]+") x)
let bool r = Arg.Unit (fun () -> r := true)
let int r = Arg.Int (fun x -> r := x)
let add_file files s = files := s :: !files
let usage = Printf.sprintf "Usage: %s [-f][-p] file\n" Sys.argv.(0)

let make () = 
(* I hate this bloody Arg module ... *)
  let prog = ref "" in
  let prog_file = ref "" in
  let files = ref [] in
  let cmxs = ref [] in
  let learn = ref "" in
  let print = ref false in
  let show_type = ref false in
  let threshold = ref 5 in
  let options = 
    ["-f", string prog_file, "filename" ;
     "-p", string prog, "inline program" ;
     "-load", string_l cmxs, "list of cmxs modules to load" ;
     "-learn", string learn, "learn a pattern from an example";
     "-print", bool print, "print program and exit" ;
     "-type", bool show_type, "print the type of the input" ;
     "-threshold", int threshold, "threshold for string type inference" ;
     "-version", Arg.Unit print_version, "prints the current version" ;
   ] in
  Arg.parse options (add_file files) usage ;
  { prog = !prog ;
    prog_file = !prog_file ;
    inline = !prog <> "" ;
    files = List.rev !files ;
    cmxs = List.rev !cmxs ;
    learn = !learn ;
    print = !print ;
    show_type = !show_type ;
    threshold = !threshold ;
    version = version ;
}
