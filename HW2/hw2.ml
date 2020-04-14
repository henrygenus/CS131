type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let first two_tuple = let ret, _ = two_tuple in ret
let second two_tuple = let _, ret = two_tuple in ret

let rec convert_rules gram1 =
  let start_symbol, rules = gram1 in
  if ((List.length rules) = 0) then [] else
    (* get rules rooted at START_SYMBOL *)
    let rules_for_symbol, rest = (List.partition (fun rule -> (first rule) = start_symbol) rules) in
    (* merge the predicates of rules for start_symbol *)
    let _, alternative_list = List.split rules_for_symbol in
    (* if no rules left to convert, return grammar 2 type rules *)
    let g2_rules = [(start_symbol, alternative_list)] in
    if ((List.length rest) = 0) then g2_rules else
      (* else recursive call on rest of rules *)
      let gram2 = (convert_rules ((first (List.hd rest)), rest)) in
      (* return appended list of grammar2 style rules *)
      g2_rules@gram2

let convert_grammar gram1 =
  let grammar_two_rules = convert_rules gram1 in
  let start_symbol = (first gram1) in
    (start_symbol, (fun rule -> List.assoc rule grammar_two_rules))

let rec parse_tree_leaves tree =
  match tree with
    | Leaf l -> [l]
    | Node (nonterminal, subtree) -> (List.concat (List.map parse_tree_leaves subtree))



(* pruning -- need to increase efficiency *)
let rec make_matcher gram =
  let (root: 'N), (get_preds : 'N -> ('N, 'T) symbol list list)  = gram in
  let preds = (get_preds root) in
  (fun accept frag -> (apply_preds (get_preds) preds accept frag))
and do_apply get_preds pred frag =
    if (pred = []) then Some frag else
    if (frag = []) then None else
      let (sym : ('N, 'T) symbol) = (List.hd pred) in
      match sym with
      | N sym -> ((make_matcher (sym, get_preds)) (do_apply get_preds (List.tl pred)) frag)
      | T sym -> if ((List.hd frag) = sym)
                 then do_apply get_preds (List.tl pred) (List.tl frag)
                 else None
and apply_preds get_preds preds accept frag =
  if (preds = []) then None else
    match (do_apply get_preds (List.hd preds) frag) with
    | None -> (apply_preds get_preds (List.tl preds) accept frag)
    | Some suffix -> let return_value = (accept suffix) in
                     if (return_value = None) then (apply_preds get_preds (List.tl preds) accept frag)
                          else return_value

(* parser calls matcher until end of fragment *)
(*let make_parser *)
