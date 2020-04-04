let rec subset a b =
  if a = [] then true else
    match (List.mem (List.hd a) b) with
    | true -> subset (List.tl a) b
    | false -> false

let equal_sets a b = (subset a b) && (subset b a)

let rec set_union a b =
  if a = [] then b else
  match (List.mem (List.hd a) b) with
    | true -> set_union (List.tl a) b
    | false -> (List.hd a)::(set_union (List.tl a) b)

let rec set_intersection a b =
  if a = [] then [] else
  match (List.mem (List.hd a) b) with
    | true -> (List.hd a)::(set_intersection (List.tl a) b)
    | false -> set_intersection (List.tl a) b

let rec set_diff a b =
  if a = [] then [] else
  match (List.mem (List.hd a) b) with
    | true -> set_diff (List.tl a) b
    | false -> (List.hd a)::(set_diff (List.tl a) b)

let rec computed_fixed_point eq (f : 'a->'a) (x :'a) =
  let res = (f x) in
  match (eq res x) with
  | true -> x
  | false -> (computed_fixed_point (eq) (f) res)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec make_grammar (nodes: 'N list) tree =
  match (nodes) with
  | [] -> []
  | _ -> List.cons (List.hd nodes, tree) (make_grammar (List.tl nodes) tree)

let map_and_merge_trees f (g : 'N * ('N * ('N, 'T) symbol list) list) tree =
  let rt, subtree = (f g) in set_union subtree tree

let rec filter_reachable ((rt : 'N ), (tree : ('N * ('N, 'T) symbol list) list))  =
  if (List.assoc_opt rt tree) = None then  (rt, []) else
    let clipped_tree = (List.remove_assoc rt tree) in
    let children : ('N, 'T) symbol list = List.assoc rt tree in
    let nonterminal_children = (List.filter_map (function N node -> Some node | _ -> None) (children)) in
    let filter_and_glue_subtrees_of = List.fold_right (map_and_merge_trees filter_reachable) in
    let subgrammars = make_grammar nonterminal_children clipped_tree in
    let subtree = filter_and_glue_subtrees_of (subgrammars) [(rt, children)] in
    (rt, set_intersection tree (glue_subtrees_of [(rt, clipped_tree)] subtree))
