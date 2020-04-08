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

let rec first (a, b) = a and second (a, b) = b
and equal_sec_set rule_symbol_one rule_symbol_two =
  let (_, a), (_, b) = (rule_symbol_one, rule_symbol_two) in (equal_sets a b)
and get_reachable (rules, reachable_symbols) =
  if (rules = []) then (rules, reachable_symbols) else
    let rule = (List.hd rules) in
    match (List.mem (first rule) reachable_symbols) with
    | false -> get_reachable ((List.tl rules), reachable_symbols)
    | true -> let nonterminal = List.filter_map (function N n -> Some n | T _ -> None) (second rule) in
              get_reachable ((List.tl rules), (set_union reachable_symbols nonterminal))
and filter_reachable g =
  let root, rules = g in
  (* get reachable symbols *)
  let _, reachable_symbols = computed_fixed_point equal_sec_set get_reachable (rules, [root]) in
  (* filter the rules *)
  let filter_rules = (function rule -> (List.mem (first rule) reachable_symbols)) in
  let filtered_rules = List.filter filter_rules rules in
  (root, filtered_rules)

