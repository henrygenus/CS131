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
  | false -> (computed_fixed_point (eq) (f) res)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec symbol (a, b) = a
and expression (a, b) = b
and equal_sec_set a b = (equal_sets (List.nth a 1) (List.nth b 1))
and get_reachable reachable_symbols rules =
  if (rules = [[], []]) then reachable_symbols else
    let rule = (List.hd rules) in
    match (List.mem reachable_symbols (symbol rule)) with
    | false -> get_reachable reachable_symbols (List.tl rules)
    | true -> let nonterminal = List.filter (function N _ -> true | _ -> false) (expression rule) in
              get_reachable (set_union reachable_symbols nonterminal) (List.tl rules)
and filter_reachable g =
  let root, rules = g in
  (* get reachable symbols *)
  let reachable_symbols = computed_fixed_point equal_sec_set get_reachable root rules in
  (* filter the rules *)
  let filter_rules = (function s_r -> (List.mem (List.hd s_r) (List.nth s_r 2))) in
  let filtered_rules = List.filter filter_rules (reachable_symbols, rules) in
  (root, filtered_rules)

