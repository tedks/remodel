module L = List
module H = Hashtbl
module P = Printf
module SM = Map.Make(String)

module D = Remodel_depgraph
module C = Remodel_cache

module Par = Parmap

(* found this on a blog *)
let (@@) fn x = fn x

let remodel_filter dg =
  D.filter (fun n -> match n with
  | D.Empty -> false
  | D.Node (t,_,_) -> C.stale t
  | D.Leaf s -> C.stale s) dg

let remodel_cache dg = D.iter (fun n -> match n with
  | D.Empty -> ()
  | D.Leaf s -> C.cache s
  | D.Node (t, _, _) -> C.cache t) dg

type remodel_exec_result = Success | Failure
    
let remodel_exec_command c = 
  let pst = Unix.system c in 
  match pst with
  | Unix.WEXITED i when i = 0 -> Success
  | _ -> Failure

let remodel_build dg = 
  let commands = D.foldi_preorder 
    (fun acc l n -> match n with 
    | D.Node (_, sc, _) -> begin 
      match sc with None -> acc | Some c -> (l, c)::acc
    end 
    | _ -> acc) 
    [] dg 
  in 
  (* build up command lists *)
  L.iter (fun (l,c) -> match remodel_exec_command c with
  | Success -> ()
  | Failure -> failwith (P.sprintf "Error running command %s" c)) 
    commands

let _ =
  let prog = Remodel_parse.prog Remodel_lex.file (Lexing.from_channel stdin) in
  C.init ();
  let dg = D.make_graph prog in
  print_endline "Initial graph:";
  print_endline @@ D.string_of_depgraph dg;
  let dg' = remodel_filter dg in
  print_endline "Filtered: "; print_endline @@ D.string_of_depgraph dg';
  flush_all ();
  remodel_build dg';
  remodel_cache dg'

  
