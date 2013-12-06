module L = List
module H = Hashtbl
module P = Printf

type item = {
  target : string;
  command : string option;
  deps : string list;
}

let string_of_item (i:item) = 
  let deps = L.fold_left (fun acc dep -> acc ^ "," ^ dep) "" i.deps in 
  let command = match i.command with Some c -> c | None -> "<no command>" in
  P.sprintf "<item target=%s command=%s deps=%s" 
    i.target command deps

type depgraph =
| Leaf of string 			(* filename *)
| Node of string * string option * depgraph list

let rec string_of_depgraph g = match g with 
  | Leaf fn -> P.sprintf "<File %s>" fn
  | Node (t, sc, deps) -> 
    let deps = String.concat ";" (L.map string_of_depgraph deps) in
    P.sprintf "<Node target=%s command=%s deps=[%s]>"
      t (match sc with Some c -> c | None -> "<no command>") deps

let make_graph (prods:Remodel_ast.production list) = 
  let default = ref "" in 
  let add_item g (i:item) = 
    if H.mem g i.target 
    then failwith ("Conflicting rule on target "^i.target)
    else H.add g i.target i; 
    g
  in 
  let (graph : (string, item) H.t) = 
    L.fold_left (fun graph prod -> match prod with
    | Remodel_ast.Default t -> default := t; graph
    | Remodel_ast.Bare_target (t, dps) -> 
      add_item graph {target = t; command = None; deps = dps}
    | Remodel_ast.Command_target (t, dps, c) -> 
      add_item graph {target = t; command = Some c; deps = dps}
    ) (Hashtbl.create (L.length prods)) prods
  in 
  let rec build_depgraph g t = 
    if not (H.mem g t) then Leaf t
    else 
      let i = H.find g t in
      assert (i.target = t);
      Node (t, i.command, L.map (build_depgraph g) i.deps)
  in build_depgraph graph !default

