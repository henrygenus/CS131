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

let accept_none = function | _ -> None

let append_matcher matcher1 matcher2 =
  fun accept frag -> matcher1 frag (fun suf -> m2 suf accept)

(* make_terminal_matcher:
   returns a matcher which matches a frag if the head matches a terminal *)
let make_terminal_matcher leaf =
  fun accept -> function | leaf::t -> accept t | _ -> None

(* make_and_matcher:
   returns a matcher which is the logical AND of a series of matchers *)
let rec make_and_matcher f = function
  | [] -> fun accept frag -> accept frag
  | h::t -> fun accept frag ->
            let recurse =  = fun accept -> make_and_matcher f t accept
            match h with
            | T h -> append_matchers (make_terminal_matcher (h : string)) (rest_matcher) frag
            | N h -> make_matcher (h, f) (make_and_matcher f t accept)  frag

(* make_or_matcher
 returns a matcher which is the logical OR of a series of matchers *)
and make_or_matcher f = function
  | [] -> fun accept frag ->  None
  | h::t -> fun accept frag ->
            match make_and_matcher f h accept frag with
            | None -> make_or_matcher f t accept frag
            | Some accepted_suffix -> Some accepted_suffix

(* make_matcher:
   returns a matcher which tests if any of a series of matchers passes a frag *)
and make_matcher gram =
  let root, production_function = gram in
  let make_a_matcher =  make_or_matcher production_function in
  match production_function root with
  | [] -> fun accept frag -> None
  | alt_list -> make_a_matcher alt_list
