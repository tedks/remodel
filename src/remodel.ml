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
      let level = max_int - l in 	(* iter counts up, so we flip
					   the levels *)
      let upd = if IM.mem level cmap 
	then c::IM.find level cmap else [c] in
      IM.add level upd cmap) IM.empty commands in 
  IM.iter 
    (fun l cl -> 
      let check_build res acc = 
	match acc with Failure -> acc | Success -> res in 
      let result = 			(* parallelism goes here *)
	Par.parmapfold remodel_exec_command (Par.L cl)
	  check_build Success check_build in 
      match result with 
      | Success -> ()
      | Failure -> failwith 
	(P.sprintf "Couldn't execute commands [%s] on level %d!" 
	   (String.concat "; " cl) l)
    ) cmdmap

let parse_to_depgraph entry inf = 
  let inch = match inf with None -> stdin | Some fn -> open_in fn in 
  let prog = Remodel_parse.prog Remodel_lex.file (Lexing.from_channel inch) in
  D.make_graph entry prog

let run () = 
  C.init ();
  Par.set_default_ncores (2 * (Par.get_default_ncores ()));
  let entry_point = ref None in 
  let infile = ref None in 
  let debug = ref false in 
  let args = [
    ("--debug", Arg.Set debug, "\t Enable debugging messages");
    ("--filename", Arg.String (fun s -> infile := Some s), 
     "The file to read rules from (stdin by default)");
  ] in 
  Arg.parse args (fun s -> entry_point := Some s) 
    "Remodel is a parallel replacement for Make written in OCaml.\n";
  let dg = parse_to_depgraph !entry_point !infile in
  if !debug then begin
    print_endline "Initial graph:";
    print_endline @@ D.string_of_depgraph dg
  end;
  let dg' = remodel_filter dg in
  if !debug then begin
  print_endline "Filtered: "; print_endline @@ D.string_of_depgraph dg'
  end;
  remodel_build dg';
  remodel_cache dg'

let _ = run ()
