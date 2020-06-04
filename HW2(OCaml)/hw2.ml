type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal


(* 1 *)

let first two_tuple = let ret, _ = two_tuple in ret
let second two_tuple = let _, ret = two_tuple in ret
let is_key key pair = (first pair) = key

(* convert_rules:
   fun grammarType1 -> associative_list *)
let rec convert_rules gram1 =
  let start_symbol, rules = gram1 in
  if (List.length rules) = 0 then [] else
    let rules_for_symbol, rest = List.partition (is_key start_symbol) rules in
    let _, alternative_list = List.split rules_for_symbol in
    let g2_rules = [(start_symbol, alternative_list)] in
    if (List.length rest) = 0 then g2_rules else
      g2_rules @ (convert_rules ((first (List.hd rest)), rest))

(* convert a type 1 grammar to a type 2 grammar (~ BNF -> EBNF) *)
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


(* identical to the lisp function BUTLAST *)
let rec butlast frag index =
  let rec loop frag  = function
  | 0 -> List.rev frag
  | n -> loop (List.tl frag) (n - 1)
  in loop (List.rev frag) index

(* build a tree for a given node *)
let rec build_tree make_a_tree make_rest_trees nodes = fun suf ->
  match suf, nodes with
  | [], [] -> Some []
  | [], _  -> None
  |  _, [] -> None
  | _, h::t ->
     match make_rest_trees suf t h with
     | None -> None
     | Some trees ->
        match make_a_tree h (butlast suf (List.length trees)) with
        | None -> None
        | Some tree -> Some (tree::trees)

(* returns a parser which builds parse trees for acceptable sentences *)
let rec make_parser (node, prod_fun) = fun frag ->
  let mat = function
    | T n -> make_terminal_matcher n (function _ -> Some(Leaf n))
    | N n -> make_parser (n, prod_fun)
  in let rec mrt suf rest = function
       | T n -> build_tree mat mrt rest (List.tl suf)
       | N n -> make_matcher (n, prod_fun) (build_tree mat mrt rest) suf
     in let tree_builder = fun rhs -> build_tree mat mrt rhs frag
        in match List.find_map tree_builder (prod_fun node) with
           | Some tree -> Some (Node (node,  tree))
           | None -> None
