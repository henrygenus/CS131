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
    (start_symbol, (fun rule -> List.assoc_opt rule grammar_two_rules))

