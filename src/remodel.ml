module L = List
module H = Hashtbl
module P = Printf

module D = Remodel_depgraph
module C = Remodel_cache

(* found this on a blog *)
let (@@) fn x = fn x

let remodel_filter dg =
  D.filter (fun n -> match n with
  | D.Empty -> false
  | D.Node (t,_,_) -> C.stale t
  | D.Leaf s -> C.stale s) dg

let remodel_build dg = 
  D.postorder (fun n -> match n with
  | D.Empty -> ()
  | D.Leaf f -> C.cache f
  | D.Node (t, c, ds) ->
    match c with
      Some c -> 
	let pst = Unix.system c in begin
	match pst with
	| Unix.WEXITED i when i = 0 -> 
	  C.cache t
	| _ -> failwith (P.sprintf "Error building %s via %s" t c) end
    | None -> ()) dg
    
let _ =
  let prog = Remodel_parse.prog Remodel_lex.file (Lexing.from_channel stdin) in
  List.iter (fun prod ->
    print_endline (Remodel_ast.string_of_production prod)) prog;
  C.init ();
  let dg = D.make_graph prog in
  print_endline @@ D.string_of_depgraph dg;
  let dg' = remodel_filter dg in
  print_endline "Filtered: "; print_endline @@ D.string_of_depgraph dg';
  remodel_build dg'

  
