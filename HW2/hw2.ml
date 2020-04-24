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


(* asynchronously build and run a pair of matchers in series *)
let make_in_series head_matcher make_tail_matcher tail =
  fun accept frag -> head_matcher (make_tail_matcher tail accept) frag

(* asynchronously build and run a pair of matchers in parallel *)
let make_in_parallel head_matcher make_tail_matcher tail =
  fun accept frag -> match head_matcher accept frag with
                     | Some suf -> Some suf
                     | None -> (make_tail_matcher tail) accept frag

(* returns a matcher which matches a frag if the head matches a terminal *)
let make_terminal_matcher leaf accept = function
  | [] -> None
  | h::t -> if h = leaf then accept t else None

(* returns a matcher which is the logical AND of a set of matchers *)
let rec make_and_matcher mam = function
  | [] -> fun accept frag -> accept frag
  | h::t -> let make_tail_matcher = make_and_matcher mam in
            make_in_series (mam h) make_tail_matcher t

(* returns a matcher which is the logical OR of a set of matchers *)
let rec make_or_matcher mam = function
  | [] -> fun accept frag -> None
  | h::t -> let make_tail_matcher = (make_or_matcher mam) in
            make_in_parallel (make_and_matcher mam h) make_tail_matcher t

(* returns a matcher which matches the root of a type2 grammar *)
let make_matcher (root, production_function) =
  let rec make_a_matcher = function
    | T node -> make_terminal_matcher node
    | N node -> make_or_matcher make_a_matcher (production_function node)
  in make_or_matcher make_a_matcher (production_function root)


(* 4 *)


(* capture all the subtrees of the first rhs to fully build a subtree *)
let make_nonterminal_parser f make_a_parser node =
  let rec build_subtree frag = function
    | [] -> Some []
    | h::t -> match make_matcher (h, f) (fun suf -> Some suf) frag with
              | None -> None
              | Some suffix ->
                 match build_subtree suffix t with
                 | None -> None
                 | Some tree -> Some ((make_a_parser h frag) @ tree)
  in fun frag -> List.find_map (build_subtree frag) (f node)
               (* can just use matcher to find *)

(* captures a leaf if there are no leftover character*)
let make_terminal_parser node =
  let attach_leaf = function [] -> node | _ -> None
  in make_terminal_matcher node attach_leaf

(* returns a parser which builds parse trees for acceptable sentences *)
let make_parser (root, production_function) =
  let rec make_a_parser = function
    | T node -> make_terminal_parser node
    | N node -> make_nonterminal_parser production_function make_a_parser node
  in match make_nonterminal_parsers f make_a_parser root with
     | Some tree -> Node (root, tree)
     | None -> None

