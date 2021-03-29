(* test case for subset *)
let subset_test1 = subset [4;5;6] [4;5;7;9;6;9;11]
let subset_test2 = (not (subset [4;5;6;15] [4;7;9;6;9;11]))

(*test case for equal sets*)
let equal_sets_test1 = equal_sets [1;5;7] [7;1;5]
let equal_sets_test2 = (not (equal_sets [1;3] [1;5]))
let equal_sets_test3 = equal_sets [1;1;1] [1;1;1]

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

