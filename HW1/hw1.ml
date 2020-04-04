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

(* given a list of symbols, produce (symbol, tree) grammars for each symbol *)
let rec make_grammar (nodes: 'N list) tree =
  match (nodes) with
  | [] -> []
  | _ -> List.cons (List.hd nodes, tree) (make_grammar (List.tl nodes) tree)

(*get_subtrees? etc*)
let rec filter_and_stick_subtree (operand : 'N * ('N * ('N, 'T) symbol list) list) tree =
  match (filter_reachable operand) with | _, subtree -> set_union subtree tree
and filter_reachable ((rt : 'N ), (tree : ('N * ('N, 'T) symbol list) list))  =
  if (List.assoc_opt rt tree) = None then  (rt, []) else
    let clipped_tree = (List.remove_assoc rt tree) in
    let child : ('N, 'T) symbol list = List.assoc rt tree in
    let child_N_symbols = (List.filter_map (function N node -> Some node | _ -> None) (child)) in
    let filter_and_glue = List.fold_right filter_and_stick_subtree in
    let subgrammars = make_grammar child_N_symbols clipped_tree in
    let filtered_subtrees = filter_and_glue (subgrammars) [(rt, child)] in
    (rt, set_intersection tree (filter_and_glue [(rt, clipped_tree)] filtered_subtrees))
