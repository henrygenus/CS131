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

let rec match_pred(prod_fcn : 'N -> ('N, 'T) symbol list list) pred frag =
  if (pred = []) then Some frag else if (frag = []) then None else
    let sym = (List.hd pred) in
    match sym with
    | N n -> dfs prod_fcn (prod_fcn n) (match_pred prod_fcn (List.tl pred)) frag
    | T (t : string) -> if (t = (List.hd frag))
                        then match_pred prod_fcn (List.tl pred) (List.tl frag)
                        else None
and dfs prod_fcn associative_list accept frag =
  if (associative_list = []) then None else
    let pred_result = (match_pred prod_fcn (List.hd associative_list) frag) in
    match pred_result with
    | None -> dfs prod_fcn (List.tl associative_list) accept frag
    | Some suffix -> let output = (accept suffix) in
                     if (output = None)
                     then dfs prod_fcn (List.tl associative_list) accept frag
                     else output

let rec make_matcher gram =
  let (root : 'n), (prod_fcn : 'N -> ('N, 'T) symbol list list) = gram in
  let (associative_list : ('N, 'T) symbol list list) = (prod_fcn root) in
  let matcher =  (dfs prod_fcn associative_list) in
  (matcher : ('T list -> 'T list option)  -> 'T list -> 'T list option)


(* parser calls matcher until end of fragment *)
(*let make_parser *)
