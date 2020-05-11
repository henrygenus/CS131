(* My test case is listed directly below; it is followed by provided cases *)

type my_nonterminals = A | B | C

let my_grammar =
  (A, function
   | A ->
      [[N B];
      [N B; T"+"; N A]]
   | B ->
      [[N C];
       [T"("; N A; T")"]]
   | C ->
      [[T"*"];
       [T"@"]])

let short_acceptor = (fun x -> if (List.length x) < 7 then Some x else None)
let my_frag = ["("; "*"; "+"; "@"; ")"; "+"; "@"; "+"; "("; "*"; "+"; "@"; ")"]
let my_pre = ["("; "*"; "+"; "@"; ")"; "+"; "@"]
let my_suf = ["+"; "("; "*"; "+"; "@"; ")"]

let make_matcher_test =
  make_matcher my_grammar short_acceptor my_frag = Some (my_suf)

let make_parser_test =
  match make_parser my_grammar my_pre with
  | Some tree -> (parse_tree_leaves tree) = my_pre
  | _ -> false


(* BEGIN PROVIDED CASES *)

let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let test0 =
  ((make_matcher awkish_grammar accept_all ["ouch"])
   = None)

let test1 =
  ((make_matcher awkish_grammar accept_all ["9"])
   = Some [])

let test2 =
  ((make_matcher awkish_grammar accept_all ["9";"+"; "$"; "1"; "+"])
   = Some["+"])

let test3 =
  ((make_matcher awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"])
  = None)

(* This one might take a bit longer.... *)
let test4 =
 ((make_matcher awkish_grammar accept_all
     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
      "++"; "+"; "0"])
 = Some[])

let test5 =
  (parse_tree_leaves (Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])]))
   = [3; 4; 5])


let small_awk_frag = ["$"; "1"; "++"; "-"; "2"]

let test6 =
  (make_parser awkish_grammar small_awk_frag)
  = Some (Node (Expr,
		[Node (Term,
		       [Node (Lvalue,
			      [Leaf "$";
			       Node (Expr,
				     [Node (Term,
					    [Node (Num,
						   [Leaf "1"])])])]);
			Node (Incrop, [Leaf "++"])]);
		 Node (Binop,
		       [Leaf "-"]);
		 Node (Expr,
			[Node (Term,
			       [Node (Num,
				      [Leaf "2"])])])]))

let test7 =
  match make_parser awkish_grammar small_awk_frag with
    | Some tree -> parse_tree_leaves tree = small_awk_frag
    | _ -> false

let ptest0 =
  ((make_parser awkish_grammar ["ouch"])
   = None)

let ptest1 =
  ((make_parser awkish_grammar ["9"])
   = Some (Node (Expr,
                 [Node (Term,
                        [Node (Num,
                               [Leaf "9"])])])))

let ptest2 =
  ((make_parser awkish_grammar ["9";"+"; "1"])
   = Some (Node (Expr,
                 [Node (Term,
                        [Node (Num,
                               [Leaf "9"])]);
                  Node (Binop,
                        [Leaf "+"]);
                  Node (Expr,
                        [Node (Term,
                               [Node (Num,
                                      [Leaf "1"])])])])))

let ptest3 =
  ((make_parser awkish_grammar ["$"; "1"])
  = Some (Node (Expr,
                [Node (Term,
                       [Node (Lvalue,
                              [Leaf "$";
                               Node (Expr,
                                     [Node (Term,
                                            [Node (Num,
                                                   [Leaf "1"])])])])])])))

let ptest4 =
  ((make_parser awkish_grammar ["++"; "$"; "1"]))

let ptest5 =
  ((make_parser awkish_grammar ["9"; "+"; "$"; "1"; "++"]))
