let rec subset a b =
  if a = [] then true else
    match (List.mem (List.hd a) b) with
    | false -> false
    | true -> subset (List.tl a) b

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
    if (eq res x) then x else (computed_fixed_point (eq) (f) res)


let rec filter_reachable g =
  let node, tree = g in
  let child = (List.assoc_opt tree node) in
  let trimmed_tree = (List.remove_assoc tree node) in
  match child with
  | None -> []
  | _ -> set_intersection tree
           (set_union
              ((node, child)::filter_reachable (node, trimmed_tree))
              (filter_reachable (child, tree)))
