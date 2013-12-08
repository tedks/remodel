module L = List
module H = Hashtbl
module P = Printf
module IM = Map.Make(struct type t = int let compare = compare end)

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
  let commands = D.foldi_postorder
    (fun acc l n -> match n with 
    | D.Node (_, sc, _) -> begin 
      match sc with None -> acc | Some c -> (l, c)::acc
    end 
    | _ -> acc) 
    [] dg 
  in 
  let cmdmap = 
    L.fold_left (fun cmap (l, c) ->
      let level = max_int - l in 
      let upd = if IM.mem level cmap 
	then c::IM.find level cmap else [c] in
      IM.add level upd cmap) IM.empty commands in 
  IM.iter 
    (fun l cl -> 
      let check_build res acc = 
	match acc with Failure -> acc | Success -> res in 
      let result = 			(* parallelism goes here *)
	Par.parmapfold ~ncores:16 remodel_exec_command (Par.L cl)
	  check_build Success check_build in 
      match result with 
      | Success -> ()
      | Failure -> failwith 
	(P.sprintf "Couldn't execute commands [%s] on level %d!" 
	   (String.concat "; " cl) l)
    ) cmdmap
  
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
      
