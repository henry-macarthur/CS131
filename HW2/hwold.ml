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

let rec matcher fn gram accept frag state = 
    if ((List.length gram) = 0) then  (*if we exhaust all the rules then we can return none as there is no match*)
        None
    else 
        let head = List.hd gram in (*get the first rule*)
        let tail = List.tl gram in (*get the rest of the rules*)
        match (recursive_matcher head fn accept frag state) with (*try the current rule on the fragment to see if there is a match*) 
        | None  -> (matcher fn tail accept frag state) (*if there is no match then we try the next rule*)
        | Some x -> Some x (*if there is a match, return what the spec says*)

and recursive_matcher current_rule fn accept frag state = 
    if ((List.length current_rule) = 0) then (*if we have no rules left in the current list we know we exhausted the current rule*)
        match state with 
        | [] -> accept frag (*this means that we have matched a rule fully with part of the fragment*)
        | head::tail -> (recursive_matcher head fn accept frag tail) (*if there are still rules left in the state, we have to look through them*)
    else 
        let first = List.hd current_rule in (*first term in the rule*)
        let rest = List.tl current_rule in (*rest of the terms in the rule*)
        match first with 
        |   N nonT -> matcher fn (fn nonT) accept frag (rest::state)(*if we find a nonterminal, we call matcher on all the rules it maps to to try and match it with a terminal*)
        (*we also add the rest of the items of the list to the current state so that if this nonterm yeilds a match, we look through the rest of the rule and try and match*)
        |   T term -> 
            match frag with 
            |   [] -> None (*if we have a terminal and the fragment is empty, it means that the rule is too long for the fragment -> bad*)
            |   head1::tail1 -> 
                if (head1 = term) then (*if the terminal matches the current term*)
                    recursive_matcher rest fn accept tail1 state (*we try and match the rest of the terms in the rule *)
                else 
                    None
and save_state x = recursive_matcher x
;;



(* let make_matcher gram = 
    let lhs = fst gram in (* start *)
    let rhs = snd gram in (* func *)
    let lst = (rhs lhs) in
    (fun acct fg -> (matcher rhs lst acct fg []));; *)

   
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
(* 
let make_matcher gram = 
    let lhs = fst gram in (* start *)
    let rhs = snd gram in (* func *)
    let lst = ((snd gram) (fst gram)) in
    (fun acct fg -> (recursive_make_match lhs rhs lst acct fg));; *)