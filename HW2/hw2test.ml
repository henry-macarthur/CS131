let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

(* An example grammar for a small subset of Awk.
   This grammar is not the same as Homework 1; it is
   instead the same as the grammar under
   "Theoretical background" above.  *)





type my_nonterminals =
  | Animal | Conj | Verb | Sentence | Something | Food

let my_grammar =
  (Sentence,
   function
     | Sentence ->
         [[N Animal; N Verb; N Conj]]
     | Conj ->
	 [ 
     [N Something];
     [N Something; T"and"; N Sentence]
     
	  ]
    | Something -> 
    [
      [N Food];
      [N Animal]
    ]
     | Food ->
	 [[T"Plants"];[T"goldfish"];[T"pizza"];[T"cereal"];[T"oreos"];[T"chips"]]
     | Animal ->
	 [[T"Dog"];
	  [T"Bird"];
    [T"Cat"];
    [T"Fish"]]
    
    | Verb -> 
    [[T"loves"]; [T"hates"]; [T"enjoys"]; [T"eats"]])


let t6 = ["Bird"; "Cat"]
let t2 = ["Bird"; "loves"; "goldfish"; "and"; "Fish"; "hates"; "chips"; "and"; "Cat"; "eats"; "oreos"]
let t3 = ["Fish"; "hates";  "pizza"; "and"; "Cat"; "eats"; "oreos"]

let make_matcher_test = ((make_matcher my_grammar accept_all t3) = Some ["and"; "Cat"; "eats"; "oreos"])

let make_parser_test = match (make_parser my_grammar t2) with 
| Some x -> ( (parse_tree_leaves x) = t2)
| None -> false
