module A = Remodel_ast
module D = Remodel_depgraph

let expect condition message = if not condition then failwith message

let () =
  let productions = [
    A.Default "all";
    A.Bare_target ("all", ["build"]);
    A.Command_target ("build", ["input.txt"], "cp input.txt build");
  ] in
  let graph = D.make_graph None productions in
  let visit acc depth node =
    let label = match node with
      | D.Node (target, _, _) -> target
      | D.Leaf filename -> filename
      | D.Empty -> "empty"
    in (depth, label) :: acc
  in
  let preorder = List.rev (D.foldi_preorder visit [] graph) in
  expect (preorder = [(0, "all"); (1, "build"); (2, "input.txt")])
    "preorder traversal did not follow dependencies";
  let postorder = List.rev (D.foldi_postorder visit [] graph) in
  expect (postorder = [(2, "input.txt"); (1, "build"); (0, "all")])
    "postorder traversal did not put dependencies first";
  let selected = D.make_graph (Some "build") productions in
  expect (D.string_of_depgraph selected =
    "<Node target=build command=cp input.txt build deps=[<File input.txt>]>")
    "explicit entry did not select build";
  let duplicate = [
    A.Bare_target ("all", []);
    A.Bare_target ("all", ["other"]);
  ] in
  let rejected = try ignore (D.make_graph None duplicate); false
    with Failure message -> message = "Conflicting rule on target all" in
  expect rejected "duplicate target was accepted"
