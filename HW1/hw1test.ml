let my_F_test0 = subset [3; 8] [1; 2; 3; 5; 8; 13; 21]
let my_F_test1 = equal_sets [3; 7; 1; 5] [1; 2; 3; 7]
let my_F_test2 = equal_sets (set_union [2; 4; 6; 8] [1; 3; 5; 7; 9]) [1; 2; 3; 4; 5; 6; 7; 8; 9]
let my_F_test3 = equal_sets (set_intersection [3; 5; 6; 7; 9] [2; 9; 8; 6; 2]) [6; 9]
let my_F_test4 = equal_sets (set_diff [3; 5; 2; 6; 8; 9] [2; 3; 0; 5; 8]) [6; 9]
let my_F_test5 = computed_fixed_point (=) (function x -> sqrt x) 100000. = 1.

type sentence_nonterminals =  Sentence | Noun_Phrase | Verb_Phrase
let good_phrases =
  [Sentence, [N Sentence; T"Conjunction"; N Sentence];
   Sentence, [N Noun_Phrase; N Verb_Phrase];
   Noun_Phrase, [T"Noun"];
   Noun_Phrase, [T"Adjective"; N Noun_Phrase];
   Verb_Phrase, [T"Verb"];
   Verb_Phrase, [N Verb_Phrase; T"Adverb"]]
let bad_phrases =
    [Sentence, [N Sentence; T"Conjunction"; N Sentence];
     Sentence, [];
     Noun_Phrase, [T"Noun"];
     Noun_Phrase, [T"Adjective"; N Noun_Phrase];
     Verb_Phrase, [T"Verb"];
     Verb_Phrase, [N Verb_Phrase; T"Adverb"]]


let good_grammar = Sentence, good_phrases
let bad_grammar = Sentence, bad_phrases


let my_F_test6 =
  filter_reachable good_grammar = good_grammar

let my_F_test7 =
  filter_reachable bad_grammar = (Sentence,
                                  [Sentence, [N Sentence; T"Conjunction"; N Sentence];
                                   Sentence, []])
