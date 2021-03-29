let subset_test0 = subset [] [1;2;3]
let subset_test1 = subset [3;1;3] [1;2;3]
let subset_test2 = not (subset [1;3;7] [4;1;3])

let equal_sets_test0 = equal_sets [1;3] [3;1;3]
let equal_sets_test1 = not (equal_sets [1;3;4] [3;1;3])

let set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3]
let set_union_test2 = equal_sets (set_union [] []) []

let set_symdiff_test0 =
  equal_sets (set_symdiff [] [1;2;3]) [1;2;3]
let set_symdiff_test1 =
  equal_sets (set_symdiff [3;1;3] [1;2;3]) [2]
let set_symdiff_test2 =
  equal_sets (set_symdiff [1;2;3;4] [3;1;2;4]) []

let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0
let computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let computed_fixed_point_test2 =
  computed_fixed_point (=) sqrt 10. = 1.
let computed_fixed_point_test3 =
  ((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			 (fun x -> x /. 2.)
			 10.)
   = 1.25)
   
type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

let awksub_test0 =
  filter_reachable awksub_grammar = awksub_grammar

let awksub_test1 =
  filter_reachable (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules)

let awksub_test2 =
  filter_reachable (Lvalue, awksub_rules) = (Lvalue, awksub_rules)

let awksub_test3 =
  filter_reachable (Expr, List.tl (List.tl awksub_rules)) =
    (Expr,
     [Expr, [N Expr; N Binop; N Expr];
      Expr, [N Lvalue];
      Expr, [N Incrop; N Lvalue];
      Expr, [N Lvalue; N Incrop];
      Lvalue, [T "$"; N Expr];
      Incrop, [T "++"];
      Incrop, [T "--"];
      Binop, [T "+"];
      Binop, [T "-"]])

let awksub_test4 =
  filter_reachable (Expr, List.tl (List.tl (List.tl awksub_rules))) =
    (Expr,
     [Expr, [N Lvalue];
      Expr, [N Incrop; N Lvalue];
      Expr, [N Lvalue; N Incrop];
      Lvalue, [T "$"; N Expr];
      Incrop, [T "++"];
      Incrop, [T "--"]])

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]]

let giant_test0 =
  filter_reachable giant_grammar = giant_grammar

let giant_test1 =
  filter_reachable (Sentence, List.tl (snd giant_grammar)) =
    (Sentence,
     [Quiet, []; Grunt, [T "khrgh"]; Shout, [T "aooogah!"];
      Sentence, [N Quiet]; Sentence, [N Grunt]; Sentence, [N Shout]])

let giant_test2 =
  filter_reachable (Quiet, snd giant_grammar) = (Quiet, [Quiet, []])

  (* test case for subset *)
let subset_test1 = subset [4;5;6] [4;5;7;9;6;9;11]
let subset_test2 = (not (subset [4;5;6;15] [4;7;9;6;9;11]))
let subset_test3 = subset [] []
let subset_test4 = subset [1; 6; 6; 6; 7] [7; 6; 1; 1]
let subset_test5 = not (subset [3; 4; 5; 6] [5; 6; 7])


(*test case for equal sets*)
let equal_sets_test1 = equal_sets [1;5;7] [7;1;5]
let equal_sets_test2 = (not (equal_sets [1;3] [1;5]))
let equal_sets_test3 = equal_sets [1;1;1] [1;1;1]
let equal_sets_test4 = equal_sets [] []
let equal_sets_test5 = not (equal_sets [6; 6; 6] [1; 2; 3])
let equal_sets_test6 = equal_sets [4; 5; 5; 6; 6] [4; 4; 5; 6]



(*test set union*)
let set_union1 = (equal_sets (set_union [1;2;3] [1;2]) [1;2;3])
let set_union2 = (equal_sets (set_union [1;2;3;4;5] [6;7]) [1;2;3;4;5;6;7])
let set_union2 = (equal_sets (set_union [] [6;7]) [6;7])

(*test symm diff *)
let sym_diff1 = equal_sets (set_symdiff [1;2;3] [4;5;6]) [1;2;3;4;5;6]
let sym_diff2 = equal_sets (set_symdiff [1;2;2] [2; 1; 1]) []

(*computed fixed point*)
 
let comp_fixed_point = computed_fixed_point (=) (fun x -> x) 40 = 40
let comp_fixed_point = (equal_sets (computed_fixed_point equal_sets (fun x -> 1::x) [2;3;4]) [1;2;3;4])

(* equal_sets (equal_sets (fun x -> 1::x) [2;3;4]) [1;2;3;4]; *)
(*  
    S -> S a 
    S -> T z
    Y -> x 
    

*)

type non_term = A | B | C | D
let test_rules = 
[
    A, [N B; T"a"];
    A, [N C; N B; T"z"];
    B, [N C; T"f"];
    C, [T"@"];
    D, [N C; T"!"]
]

let gram = A, test_rules

let filter_test_1 = not (filter_reachable gram = (A, 
[
    A, [N B; T"a"];
    A, [N C; N B; T"z"];
    B, [N C; T"f"];
    C, [T"@"];
    D, [N C; T"!"]
])
)

let filter_test_2 = filter_reachable gram = (A, 
[
    A, [N B; T"a"];
    A, [N C; N B; T"z"];
    B, [N C; T"f"];
    C, [T"@"];
])

let test_rules2 = 
[
    A, [N D; T"a"];
    A, [N C; N B; T"z"];
    B, [N C; T"f"];
    C, [T"@"];
    D, [N C; T"!"]
]

let gram2 = A, test_rules2

let filter_test_2 = filter_reachable gram2 = (A, 
[
    A, [N D; T"a"];
    A, [N C; N B; T"z"];
    B, [N C; T"f"];
    C, [T"@"];
    D, [N C; T"!"]
])

let gram3 = A, List.tl test_rules2
let filter_test_3 = filter_reachable gram3 = (A, 
[
    A, [N C; N B; T"z"];
    B, [N C; T"f"];
    C, [T"@"];
])
let subset_test1 = subset [] [];;
let subset_test2 = subset [1;2;3] [1;2;2;3;3;4]
 
let equal_sets_test1 = equal_sets [] []
let equal_sets_test2 = equal_sets [1; 1; 1; 1; 1; 2;] [1; 2]
 
let set_union_test1 = not(equal_sets (set_union [1; 1] [2]) [1])
let set_union_test2 = equal_sets (set_union [1;2;3] [2;3;4]) [1;2;3;4]
 
let set_symdiff_test1 =
   equal_sets (set_symdiff [1; 1; 1] [1]) []
let set_symdiff_test2 =
   equal_sets (set_symdiff [1; 2; 3; 4] [1; 1; 2; 2; 3; 3;]) [4]
 
let computed_fixed_point_test1 = computed_fixed_point equal_sets (fun x -> List.rev x) [1; 2; 3; 4; 5] = [1; 2; 3; 4; 5]
let computed_fixed_point_test2 = computed_fixed_point equal_sets (fun x -> List.filter (fun y -> y < 5) x) [4; 3; 2; 6; 7; 7] = [4; 3; 2]
 
type human_nonterminals =
   | Conversation | Speak | Angry | Sad | Happy
 
let human_grammar =
   Conversation,
   [Happy, [T"YAAAY"];
   Sad, [T"waaaaaa"];
   Angry, [T"ughh!"];
   Speak, [N Happy];
   Speak, [N Angry];
   Speak, [N Sad];
   Conversation, [N Speak];
   Conversation, [N Speak; N Conversation; N Speak];
   Conversation, []]
 
let human_test1 = filter_reachable human_grammar = human_grammar
let human_test2 = filter_reachable (Sad, List.tl (snd human_grammar)) = (Sad, [Sad, [T"waaaaaa"]])
 
let human_test3 = filter_reachable (Speak, (snd human_grammar)) = (Speak,
  [(Happy, [T "YAAAY"]); (Sad, [T "waaaaaa"]); (Angry, [T "ughh!"]);
   (Speak, [N Happy]); (Speak, [N Angry]); (Speak, [N Sad])])

let my_subset_test0 =
    subset [1;3;5] [5;1;3;6;7;2] = true

let my_subset_test1 =
    subset [1;3;5;9] [5;1;3;6;7;2] = false

let my_equal_sets_test0 =
    equal_sets [1;3;5;9] [9;1;3;5] = true

let my_equal_sets_test1 =
    equal_sets [1;3;5;9] [5;1;3;6;7;2] = false

let my_equal_sets_test2 =
    equal_sets [1;3;5;9] [] = false

let my_equal_sets_test3 =
    equal_sets [] [] = true

let my_set_union_test0 =
    equal_sets ([3;4;5;9]) (set_union [4;3;5] [3;4;5;9])

let my_set_union_test1 =
    set_union [] [] = []

let my_set_symdiff_test0 =
    equal_sets ([9]) (set_symdiff [4;3;5] [3;4;5;9])

let my_set_symdiff_test1 =
    equal_sets ([]) (set_symdiff [4;3;5] [3;4;5])

let my_set_symdiff_test2 =
    equal_sets ([6;7]) (set_symdiff [4;3;5;7] [3;4;5;6])

let my_computed_fixed_point_test0 =
    computed_fixed_point (=) (fun var -> var / var ) 1000 = 1;;

let my_computed_fixed_point_test1 =
    computed_fixed_point (=) (fun var -> var mod 5) 33= 3;;

type food_nonterminals =
    | Apple | Orange | Kiwi | Banana;;

let food_rules = 
    [Apple,[T"green";N Kiwi];
    Apple,[N Orange;T"orange"];
    Apple,[N Kiwi];
    Apple,[N Banana;T"yellow"];
    Orange,[N Banana];
    Kiwi,[N Orange];
    Banana,[N Orange; T"fruits"]
    ];;

let food_grammar = Apple,food_rules;;
let my_filter_reachable_test0 = filter_reachable food_grammar = food_grammar;;

let food_grammar = Orange,food_rules;;
let food_rules_orange_start = 
    [Orange,[N Banana];
    Banana,[N Orange; T"fruits"]
    ];;
let orange_answer=Orange,food_rules_orange_start;;
let my_filter_reachable_test0 = filter_reachable food_grammar =
    orange_answer;;





let subsetTest = subset [8;24] [81;24;8;96]
let subsetTest2 = not (subset [81;24;8;96] [8;24])

let equalSetsTest = equal_sets [8;24;24] [24;8]
let equalSetsTest2 = not (equal_sets [8;24;81] [8;24;96])

let setUnionTest = equal_sets (set_union [8;24] [8;81;96]) [8;24;81;96]
let setUnionTest2 = equal_sets (set_union [] [8;24]) [8;24]

let setSymdiffTest = equal_sets (set_symdiff [8;24] [81;96]) [8;24;81;96]
let setSymdiffTest2 = equal_sets (set_symdiff [8;24;81;96] [8;24;5]) [5;81;96]

let computedFixedPointTest = computed_fixed_point (=) (fun x -> x / 100) 1000000 = 0
let computedFixedPointTest2 = computed_fixed_point (=) (fun x -> x mod 2) 54321 = 1
let computedFixedPointTest3 = computed_fixed_point (=) (fun x -> x *. 19. ) 12. = infinity

type nonTerminals =
  | Kobe | Magic | Shaq | Kareem | Lebron

let rules = 
    [Magic, [N Kobe; N Lebron; T"L"];
    Magic, [N Shaq];
    Magic, [N Kareem];
    Kobe, [T"5"; N Lebron];
    Kobe, [N Shaq];
    Lebron, [N Kobe];
    Lebron, [T"4"];
    Lebron, [N Kareem];
    Kareem, [N Shaq];
    Shaq, [T"4"]]

let grammar = Magic, rules

let filterReachableTest = filter_reachable grammar = grammar

let grammar2 = Kobe, rules

let filterReachableTest2 = filter_reachable grammar2 = 
    (Kobe, 
    [Kobe, [T"5"; N Lebron];
    Kobe, [N Shaq];
    Lebron, [N Kobe];
    Lebron, [T"4"];
    Lebron, [N Kareem];
    Kareem, [N Shaq];
    Shaq, [T"4"]]
    )

let my_subset_test0 = subset [5;5] [1;2;3;4;5]

let my_equal_sets_test0 = equal_sets [5] [5;5]

let my_set_union_test0 = equal_sets (set_union [5] [5]) [5;5]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x mod 2) 1234567 = 1

type test_nonterminals = 
    | Meal | Main_Course | Desert | Appetizer | Food | Ingredients

let test_rules = 
[Meal, [N Appetizer; N Main_Course; N Desert];
 Meal, [N Appetizer; N Main_Course];
 Meal, [N Main_Course];
 Appetizer, [N Food];
 Main_Course,[N Food];
 Desert, [N Food];
 Food, [N Ingredients];
 Ingredients, [T"Chicken"];
 Ingredients, [T"Salad"];
 Ingredients, [T"Ice Cream"];
 Ingredients, [T"Mashed Potatoes"];]

let test_grammer = Appetizer, test_rules

let my_filter_reachable_test0 = 
 filter_reachable test_grammer = 
  (Appetizer, [Appetizer, [N Food];
  Food, [N Ingredients];
  Ingredients, [T"Chicken"];
  Ingredients, [T"Salad"];
  Ingredients, [T"Ice Cream"];
  Ingredients, [T"Mashed Potatoes"];])

(* tests for filter_unreachable using animal_grammar *)
type animal_nonterminals = 
    | Animal | Pet| NotPet | Cat | Dog | Elephant | Zebra

let animal_grammar =
    Animal, 
    [Animal, [N Pet; T"Attacks"; N NotPet];
    Cat, [T "Meow"];
    Dog, [T "Bark"];
    Pet, [N Cat; N Dog];
    NotPet, [N Elephant];
    Elephant, [T"IDK"];
    Zebra, [T"yep"; N Dog]
    ]

let revised_animal_grammar = 
    Animal, 
    [Animal, [N Pet; T"Attacks"; N NotPet];
    Cat, [T "Meow"];
    Dog, [T "Bark"];
    Pet, [N Cat; N Dog];
    NotPet, [N Elephant];
    Elephant, [T"IDK"];
    ]

let my_animal_test0 = 
    filter_reachable animal_grammar = revised_animal_grammar

let my_animal_test1 = 
    filter_reachable (Zebra, (snd animal_grammar)) = (Zebra, [Dog, [T "Bark"]; Zebra, [T"yep"; N Dog]])

