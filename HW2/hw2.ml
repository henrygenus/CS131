type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let first two_tuple = let ret, _ = two_tuple in ret
let second two_tuple = let _, ret = two_tuple in ret
let is_key key pair = (first pair) = key

let rec convert_rules gram1 =
  let start_symbol, rules = gram1 in
  if ((List.length rules) = 0) then [] else
    (* get rules rooted at START_SYMBOL *)
    let rules_for_symbol, rest = List.partition (is_key start_symbol) rules in
    (* merge the predicates of rules for start_symbol *)
    let _, alternative_list = List.split rules_for_symbol in
    (* if no rules left to convert, return grammar 2 type rules *)
    let g2_rules = [(start_symbol, alternative_list)] in
    if ((List.length rest) = 0) then g2_rules else
      (* else recursive call on rest of rules *)
      g2_rules@(convert_rules ((first (List.hd rest)), rest))

let convert_grammar gram1 =
  let grammar_two_rules = convert_rules gram1 in
  let start_symbol = (first gram1) in
    (start_symbol, (fun rule -> List.assoc rule grammar_two_rules))

let rec parse_tree_leaves tree =
  match tree with
    | Leaf l -> [l]
    | Node (_, subtree) -> (List.concat (List.map parse_tree_leaves subtree))

let rec complete_match prod_fcn pred frag =
  let suf = match_pred prod_fcn pred frag in
  match suf with
  | Some [] -> Some []
  | None -> None
  | _ -> None
and match_pred (prod_fcn : 'N -> ('N, 'T) symbol list list) pred frag =
  if (pred = []) then Some frag else if (frag = []) then None else
    match pred with
    | (T (h : string))::t  -> if (h <> (List.hd frag)) then None
                              else match_pred prod_fcn t (List.tl frag)
    | (N h)::t -> let match_full_suffix = complete_match prod_fcn t in
                  dfs prod_fcn (prod_fcn h) match_full_suffix frag
and dfs prod_fcn assoc_list accept frag =
  if (assoc_list = []) then None else
    match (match_pred prod_fcn (List.hd assoc_list) frag) with
    | None -> dfs prod_fcn (List.tl assoc_list) accept frag
    | Some suffix -> match (accept suffix) with
                     | Some output -> Some output
                     | None ->  dfs prod_fcn (List.tl assoc_list) accept frag

let rec make_matcher gram =
  let (root : 'n), (prod_fcn : 'N -> ('N, 'T) symbol list list) = gram in
  let (associative_list : ('N, 'T) symbol list list) = (prod_fcn root) in
  let matcher =  (dfs prod_fcn associative_list) in
  (matcher : ('T list -> 'T list option)  -> 'T list -> 'T list option)


(* parser calls matcher until end of fragment *)
(* let make_parser *)
