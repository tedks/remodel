module L = List
module H = Hashtbl
module P = Printf

module Depgraph = Remodel_depgraph

(* found this on a blog *)
let (@@) fn x = fn x

let _ = 
  let prog = Remodel_parse.prog Remodel_lex.file (Lexing.from_channel stdin) in 
  List.iter (fun prod -> 
    print_endline (Remodel_ast.string_of_production prod)) prog;
  print_endline @@ Depgraph.string_of_depgraph @@ Depgraph.make_graph prog
  
;;

  
