type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let first two_tuple = let ret, _ = two_tuple in ret
let second two_tuple = let _, ret = two_tuple in ret
let is_key key pair = (first pair) = key


(* 1 *)

(* convert_rules:
   fun grammarType1 -> associative_list *)
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

(* convert_grammar:
   grammarType1 -> grammarType2 *)
let convert_grammar gram1 =
  let grammar_two_rules = convert_rules gram1 in
  let start_symbol = (first gram1) in
    (start_symbol, (fun rule -> List.assoc rule grammar_two_rules))


(* 2 *)

(* parse_tree_leaves:
   fun parse_tree -> terminal symbol list *)
let rec parse_tree_leaves tree =
  match tree with
    | Leaf l -> [l]
    | Node (_, subtree) -> (List.concat (List.map parse_tree_leaves subtree))


(* 3 *)


(* make_terminal_matcher:
   fun terminal -> (head = terminal) && accept tail *)
let make_terminal_matcher make_a_matcher =
  fun leaf -> fun accept frag ->  match frag with | [] -> None | leaf::t -> accept t

(* make_AND_matcher:
   fun predicate -> (match (first predicate) && make_AND_matcher (rest predicate)) && accept suffix *)
let rec make_and_matcher make_a_matcher  = function
  | [] -> fun accept frag -> accept frag
  | h::t -> let and_rest = make_and_matcher make_a_matcher t in
            fun accept frag -> make_a_matcher h (and_rest accept) frag

(* make_OR_matcher:
   fun associative_list -> any(any(match prefix && accept suffix) in predicate) in associate_list) *)
let rec make_or_matcher make_a_matcher = function
  | [] -> fun accept frag -> accept frag
  | h::t ->  let or_rest = make_or_matcher make_a_matcher t in
             fun accept frag -> match make_a_matcher h accept frag with
                                | Some orMatch -> orMatch
                                | None -> or_rest accept frag

(* make_matcher:
   fun grammarType2 -> (fun accept frag -> any(matcher prefix accept suffix) in frag) *)
let make_matcher gram =
  let root, production_function = gram in
  match production_function root with
  | [] -> fun accept frag -> None
  | alternative_list ->
     let rec make_a_matcher = function
       | [] -> fun accept frag -> accept frag
       | rhs -> match List.hd rhs with
                | T h -> fun accept frag -> make_terminal_matcher make_and_matcher rhs accept frag
                | N h -> fun accept frag -> make_and_matcher make_a_matcher rhs accept frag
     in make_or_matcher make_a_matcher alternative_list accept frag
