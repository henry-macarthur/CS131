
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type ('nonterminal, 'terminal) symbol = 
    | N of 'nonterminal
    | T of 'terminal;;

let isN a = match a with 
    | Node _  -> true 
    | _ -> false;;

let isL a = match a with 
    | Leaf terminal -> true
    | _ -> false;;

let compare_types left right = 
    (left = right);;

let return_second item = 
    snd item;;
(*pass in a list of rules and find all of them that have non_term on the lhs*)
let match_rules rules non_term = 
    let l1 = (List.filter(fun i -> (compare_types (fst i)  non_term)) rules) in 
    let l2 = (List.map return_second l1) in l2;;


(* return a tuple such that the first element is the start symbol and the second element is a function 
that given a nonterimal input,  returns a list of all the rules it maps to *)
let convert_grammar (start_symbol, rules) =  
    let find_matching_rules x = match_rules rules x in (start_symbol, find_matching_rules);;



let rec traverseTree nodes = match nodes with 
| [] -> [];
| _ -> 
    let item = List.hd nodes in 
    let rest = List.tl nodes in
    if(isN item) then
        match item with 
        | Node (a, b) -> (traverseTree b) @ (traverseTree rest)
        (* | LE -> []@(traverseTree rest) *)
    else
        match item with 
        | Leaf n -> [n]@(traverseTree rest);;
    

let parse_tree_leaves tree =
    match tree with 
    | Node (a, b) -> traverseTree b
    | Leaf a -> [a]
    | _ -> [];;


let rec iterate_rules get_rhs cur_rules accept fragment =
    match cur_rules with 
    | [] -> None (*used up all the rules*)
    | current_rule::tail -> (*we have rules to explore, we want to explore rule by rule *)
        match (try_rule get_rhs current_rule accept fragment) with 
        | None -> (iterate_rules get_rhs tail accept fragment)(*current rule didnt work so we try the next one*)
        | Some x -> Some x 

and match_item get_rhs cur_item accept_tail fragment = (*this function is used to match a given item in the rule *)
    match cur_item with (*look at the  item *)
    | N nonTerm -> (*if it is a n we have to do some more looking*)
        (iterate_rules get_rhs (get_rhs nonTerm) accept_tail fragment) (*iterate through its rules to see if we find something good*)
    | T term -> match_terminal fragment term accept_tail

and  try_rule get_rhs cur_rule accept fragment = 
    match cur_rule with 
    | [] -> accept fragment
    | rule1::rest_of_rules -> (*get the first item and the rest*)
        let accept_tail = (try_rule get_rhs rest_of_rules accept) in (*this is a curried function*)
        let accept_head = match_item get_rhs rule1 accept_tail fragment in (*pass in this curried function cuz if we find a match, we have to accept the tail*)
        accept_head
    
and match_terminal fragment term accept= 
    match fragment with
    | [] -> None 
    | head::tail -> 
        if (head = term) then accept tail
        else 
            None
;;


let make_matcher gram = 
    let lhs = fst gram in (* start *)
    let rhs = snd gram in (* func *)
    let lst = (rhs lhs) in
    (fun acct fg -> (iterate_rules rhs lst acct fg));;


(*--------------------------------*)



let rec traverse_rule_list fn rules make_tree cur_symbol fragment state =  
    match rules with 
    | [] -> None
    | head::tail -> 
        let try_head = match_rule fn head make_tree cur_symbol fragment state in 
        let try_tail x = traverse_rule_list fn tail make_tree cur_symbol fragment x in 
        match try_head with 
        | None -> try_tail state
        | Some x -> Some x

and match_rule fn rule make_tree cur_symbol fragment state = 
    match rule with 
    | [] -> make_tree fragment (Node (cur_symbol, state))
    | head::tail -> (*have more rules left to explore*)
        match head with 
        | N nonterminal_node -> 
            let possible_rules = (fn nonterminal_node) in
            traverse_rule_list fn possible_rules (append_acceptor fn tail make_tree cur_symbol state) nonterminal_node fragment []
        | T terminal_node -> (*if we fine a terminal node and it matches what we are looking for we are going to want to add it*)
            let len = List.length fragment in 
            if (len != 0) then 
                let frag_head = List.hd fragment in 
                let frag_tail = List.tl fragment in 
                (* now compare the fragment to the rule *)
                if (terminal_node = frag_head) then(*if they match*)
                    let temp_node = [(Leaf terminal_node)] in (*create a terminal node to add*)
                    match tail with 
                    | [] -> let tmp = (Node (cur_symbol, (state @ temp_node))) in (make_tree frag_tail tmp)
                    | _::_ -> match_rule fn tail make_tree cur_symbol frag_tail (state @ temp_node)
                else
                    None (*if they do not match we just backtrack*)
            else 
                None
and append_acceptor fn rule make_tree cur_symbol state x y = 
    match rule with 
    | [] -> let  tmp  = (Node (cur_symbol, state @ [y])) in ( make_tree x tmp)
    | _ -> match_rule fn rule make_tree cur_symbol x (state @ [y])
;; 

let make_parser gram = 
    let lhs = fst gram in 
    let rhs = snd gram in 
    let lst = (rhs lhs) in 
    let state = [] in 
    let accept_tree frag tree = match frag with  
    | [] -> Some tree
    | _ -> None in 
    (fun fg -> traverse_rule_list rhs lst accept_tree lhs fg state);;