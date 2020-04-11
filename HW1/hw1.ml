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

let rec computed_fixed_point eq f x =
  let res = (f x) in
  match (eq res x) with
  | true -> x
  | false -> (computed_fixed_point eq f res)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

<<<<<<< HEAD
(* given a list of symbols and a shared tree, produce (symbol, tree) grammars for each symbol *)
let rec make_grammar (nodes: 'N list) tree =
  match (nodes) with
  | [] -> []
  | _ -> List.cons (List.hd nodes, tree) (make_grammar (List.tl nodes) tree)
(* glue subtrees of operand to tree *)
and filter_and_stick_subtree (operand : 'N * ('N * ('N, 'T) symbol list) list) tree =
  match (filter_reachable operand) with | _, subtree -> set_union subtree tree
and filter_reachable ((rt : 'N ), (tree : ('N * ('N, 'T) symbol list) list))  =
  if (List.assoc_opt rt tree) = None then  (rt, []) else
    (* get symbols of the child expression & nonterminal symbols *)
    let child_expr : ('N, 'T) symbol list = List.assoc rt tree in
    let expr_N_symbols = (List.filter_map (function N node -> Some node | _ -> None) (child_expr)) in
    (* seperate found child subtree from rest of tree *)
    let clipped_tree = (List.remove_assoc rt tree) in
    (* glue trees of all subtrees of child together *)
    let filter_and_glue = List.fold_right filter_and_stick_subtree in
    let subgrammars = make_grammar expr_N_symbols clipped_tree in
    let filtered_subtrees = filter_and_glue (subgrammars) [(rt, child_expr)] in
    (* recursive call on same node to check for other children, merge with child subtrees *)
    (* set_intersection used to reset order *)
    (rt, set_intersection tree (filter_and_glue [(rt, clipped_tree)] filtered_subtrees))
=======
let rec first (a, b) = a and second (a, b) = b
and equal_sec_set rule_symbol_one rule_symbol_two =
  let (_, a), (_, b) = (rule_symbol_one, rule_symbol_two) in (equal_sets a b)
and get_reachable (rules, reachable_symbols) =
  if (rules = []) then (rules, reachable_symbols) else
    let rule = (List.hd rules) in
    match (List.mem (first rule) reachable_symbols) with
    | true -> let nonterminal = List.filter_map (function N n -> Some n | T _ -> None) (second rule) in
              get_reachable ((List.tl rules), (set_union reachable_symbols nonterminal))
    | false -> let unreached_rules, reached_symbols = get_reachable ((List.tl rules), reachable_symbols) in
               (rule::unreached_rules, reached_symbols)
and filter_reachable g =
  let root, rules = g in
  (* get reachable symbols *)
  let _, reachable_symbols = computed_fixed_point equal_sec_set get_reachable (rules, [root]) in
  (* filter the rules *)
  let filter_rules = (function rule -> (List.mem (first rule) reachable_symbols)) in
  let filtered_rules = List.filter filter_rules rules in
  (root, filtered_rules)

>>>>>>> cleanup
