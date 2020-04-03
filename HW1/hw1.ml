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
  | false-> (computed_fixed_point (eq) (f) res)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec filter_reachable (g : 'N * ('N * ('N, 'T) symbol list) list)  =
  let (rt, tree) = g in
  let trimmed_tree = (List.remove_assoc rt tree) in
  if (List.assoc_opt rt tree) = None then  ( "", []) else
    let child = (List.assoc rt tree) in
    let _, sub_tree = (filter_reachable (child, trimmed_tree)) in
    let _, rem_tree = (filter_reachable (rt, trimmed_tree)) in
    ((rt, set_intersection tree (set_union sub_tree ((rt, child)::rem_tree))))

(* fold union iter filter_reachable *)
