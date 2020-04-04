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

let rec make_pair_list (keys : 'N list) value =
  match (keys) with
  | [] -> []
  | _ -> List.cons (List.hd keys, value) (make_pair_list (List.tl keys) value)

let merge_trees ((rt_one : 'N), tree_one) tree = set_union tree_one tree

let filter_and_merge_trees f (g : 'N * ('N * ('N, 'T) symbol list) list) tree = merge_trees (f g) tree

let rec filter_reachable (g : 'N * ('N * ('N, 'T) symbol list) list)  =
  let ((rt : 'N) , (tree : ('N * ('N, 'T) symbol list) list)) = g in
  if (List.assoc_opt rt tree) = None then  (rt, []) else
    let (children : ('N, 'T) symbol list) = List.assoc rt tree in
    let get_nonterminal = (function N node -> Some node | _ -> None) in
    let nonterminal = (List.filter_map get_nonterminal (children)) in
    let glue_subtrees = List.fold_right (filter_and_merge_trees filter_reachable) in
    let subtree = glue_subtrees (make_pair_list nonterminal (List.remove_assoc rt tree)) [] in
    (rt, set_intersection tree (List.cons (rt, children) subtree))
