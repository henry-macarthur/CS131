

let rec subset a b = match a with 
    [] -> true 
    | _ -> if (List.mem (List.hd a) b) then subset (List.tl a) b  else false;;


let equal_sets a b = 
    if (subset a b && subset b a) then true
    else false;;

let rec set_union a b = match a with 
    [] -> b 
    | _ -> if(List.mem (List.hd a) b) then 
        set_union (List.tl a) b
        else 
        set_union (List.tl a) (List.cons (List.hd a) b)


let find_intersect a b = (List.filter(fun i -> (List.mem i a )) b);;

(* all members of A U  B that are not in a and b  *)
let set_symdiff a b = 
    let inter = find_intersect a b 
    in 
    let union = set_union a b
    in 
    (List.filter(fun i ->  (not (List.mem i inter))) union);;

(* number 5 cannot be solved*)
(* we cannot solve the question of whether or not a set S is a member of itself, one of the 
main problems comes from the fact that ocaml is statically typed. If we represent S a list of lists (ie a set containing sets, then 
to check to see whether or not S is a member of itself, we would have to compare a type of list containing lists to a type list. Ocaml, being
statically typed, would not allow us to make a comparison between these two different types and would throw an error right awat*)

let rec computed_fixed_point eq f x = 
    if(eq x (f x)) then x 
    else computed_fixed_point eq f (f x);;


(*-----------QUESTION 7----------*)

type ('nonterminal, 'terminal) symbol = 
    | N of 'nonterminal
    | T of 'terminal;;


let equal_second (_, a1, _) (_ , b1, _) = 
    (equal_sets a1 b1);;

let isN a = match a with 
    | N nonterminal -> true
    | _ -> false;;

let isempty a = match a with 
    | [] -> true
    | _ -> false;;

let rec find_reachable (rules, reachable, start_value) = 
    match rules with 
    | [] -> (start_value, reachable, start_value)
    | _  -> 
            let tmp = List.hd rules in 
            let rest = List.tl rules in 
            let tmp_symbol =  (N (fst tmp))  in (*need to make sure we have the N in front of it *)
            let rhs = snd tmp in 
            if (List.mem tmp_symbol reachable) then
                let nonterm = (List.filter(fun i -> (isN i)) rhs) in 
                let inp = find_reachable (rest, (set_union reachable nonterm), start_value) in 
                inp
            else 
                let inp = find_reachable (rest, reachable, start_value) in 
                inp;;
    

(* return a copy of the grammar with all unreachable rules removed, preserve the order of the rule! *)
let filter_reachable g =
    let start_s = fst g 
    in 
    let grammar_rules = snd g 
    in 
    let inp = (grammar_rules, [N start_s], grammar_rules)
    in
    let reachable_symbols = computed_fixed_point equal_second find_reachable inp
    in let (_, middle_element,_) = reachable_symbols
    in (start_s, (List.filter(fun i -> (List.mem (N (fst i)) middle_element)) grammar_rules));; 


(* An example grammar for a small subset of Awk.  *)



(* let rec match_rules rules matching non_term = match rules with 
| [] -> matching
| _  -> 
        let cur = List.hd rules in 
        let rest = List.tl rules in 
        let lhs = fst cur in 
        let rhs = snd cur in (* this is a list*)
        if(compare_types lhs non_term) then (* find a matching rule *)
        (*now we need to check to see if the rhs has already been added to rules*)
            if(List.mem rhs matching) then 
                match_rules rest matching non_term
            else
                match_rules rest (matching@[rhs]) non_term
        else 
            match_rules rest matching non_term;; 
            -----------------
*)



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
    | _ -> [];;



(* let match_helper_function start fn gram accept frag = 
    recursive_make_match start fn gram accept frag;; *)

let rec recursive_make_match  fn gram accept frag = 
    match gram with 
    | [] -> None (*gone through every rule and we can no longer find a match *)
    | head::tail -> (*this is what lets us look through each rule *)
        match (recursive_find_rule head fn accept frag) with (*recursivley look through and match the rule to find terminals*)
        | None -> (recursive_make_match  fn tail accept frag) (*if the first rule doesnt work, try the next ones*)
        | Some x -> Some x (*if it does work, return what we found *)
    (* | [] -> None *)
and recursive_find_rule current fn accept frag = 
    match current with (*look at the current rule *)
    | [] -> (accept frag) (* this means that we have no more symbols left in the rule to match so we found it *)
    (*we have found a rule to match the prefix, so as the spec says, we return the accept function on the suffix*)
    | first::rest -> (*look through each T and N in the rule *)
        match first with 
        | N nonT  -> (* if we find a nonterminal it means we have to keep looking, might have to match another rule *)
            recursive_make_match fn (fn nonT) (save_state rest fn accept) frag (*keep matching the non terminals until we get a terminal*)
        | T term -> (* if we find a terminal try it compared to the current symbol *)
            match frag with 
            | [] -> None (*this means we have used up the fragment but there are still rules left we need *)
            | head1::tail1 -> (*need to make sure the current terminal matches the rule we found*)
                if(head1 = term) then(*keep matching since the terminal can be produced*)
                    recursive_find_rule rest fn accept tail1 (*look through the rest of the fragment*)
                else 
                    None (*we do not match  anything so we have to look starting from another rule *)
and save_state x = recursive_find_rule x
;;

let make_matcher gram = 
    let lhs = fst gram in (* start *)
    let rhs = snd gram in (* func *)
    let lst = ((snd gram) (fst gram)) in
    (fun acct fg -> (recursive_make_match lhs rhs lst acct fg));;

   
