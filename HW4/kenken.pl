
kenken(N, C, T) :- 
    len_row(T, N),
    len_col(T, N),
    within_domain(T, N),
    check_constraint_list(C, T),
    maplist(fd_all_different, T),
    transpose(T, Y),
    maplist(fd_all_different, Y),
    maplist(fd_labeling, T).

check_constraint_list([], T).
check_constraint_list([Hr|Lr], T) :-
    %write(Hr), nl,
    validate_constraint(T, Hr),
    check_constraint_list(Lr, T).

get_value_at_index(T, R, C, V) :-
    nth1(R, T, Ret),
    nth1(C, Ret, V).

validate_constraint(_, +(0, [])).
validate_constraint(T, +(Gn, [[R|C] | RN])) :-
    get_value_at_index(T, R, C, V),
    Gn #= V + Z,
    validate_constraint(T, +(Z, RN)).

validate_constraint(_, *(1, [])).
validate_constraint(T, *(Gn, [[R|C] | RN])) :-
    get_value_at_index(T, R, C, V),
    Gn #= V * Z, 
    validate_constraint(T, *(Z, RN)).

validate_constraint(G, -(D, [I | J], [I2 | J2])) :-
    get_value_at_index(G, I, J, V1),
    get_value_at_index(G, I2, J2, V2),
    ( D + V2 #= V1; D + V1 #= V2).

validate_constraint(T, /(Gn, [R1|C1], [R2|C2])) :- 
    get_value_at_index(T, R1, C1, V1),
    get_value_at_index(T, R2, C2, V2),
    ( Gn * V2 #= V1 ) ; ( Gn * V1 #= V2 ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

within_domain([], _).
within_domain([H | T], N) :-
    fd_domain(H, 1, N),
    within_domain(T, N).

within_domain1(N, Domain) :- 
    findall(X, between(1, N, X), Domain).

% fill in a 2D array with lists of fixed length (N)
% http://www.gprolog.org/manual/gprolog.html#sec215
fill_2d([], _).
fill_2d([Head | Tail], N) :-
    within_domain1(N, Domain),
    permutation(Domain, Head),
    fill_2d(Tail, N).


% thanks to the TA github for this starter code!
create_grid(Grid, N) :-
    length(Grid, N),
    fill_2d(Grid, N).

plain_kenken(N, C, T) :-
    create_grid(T, N),
    transpose(T, Y),
    check_each_row(Y),
    check_constraint_list1(C, T).


sum_helper(_, [], 1.0).
sum_helper(T, G, [[R|C]|L]) :-
    get_value_at_index(T, R, C, V),
    Sm = G - V,
    sum_helper(T, Sm, L).

validate_constraints(_, +(0, [])).
validate_constraints(T, +(Gn, [[R|C] | RN])) :-
    get_value_at_index(T, R, C, V),
    Z is Gn - V,
    Z >= 0,
    validate_constraint(T, +(Z, RN)).

validate_constraints(T, *(Gn, D)) :- check_mult(T, D, 1, Gn).


validate_constraints(T, /(G, [R1|C1], [R2|C2])) :- 
    %print(G),
    get_value_at_index(T, R1, C1, V1),
    get_value_at_index(T, R2, C2, V2),
    (V1 is V2 * G; V2 is V1 * G).
    %(0 #= (V1 rem V2), Gn #= V1 / V2) ; (0 #= (V2 rem V1), Gn #= V2 / V1).
    % ( V1 is G * V2; V2 is G * V1 ).

validate_constraints(G, -(D, [I | J], [I2 | J2])) :-
    get_value_at_index(G, I, J, V1),
    get_value_at_index(G, I2, J2, V2),
    ( V1 is D + V2 ; V2 is D + V1 ).


check_constraint_list1([], T).
check_constraint_list1([Hr|Lr], T) :-
    validate_constraints(T, Hr),
    check_constraint_list1(Lr, T).
    
check_mult(_, [], X, X).
check_mult(T, [[R|C] | RN], Product, GN) :- 
    get_value_at_index(T, R, C, V),
    Cur is V * Product, 
    Cur =< GN,
    0 =:= GN rem Cur,
    check_mult(T, RN, Cur, GN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
check_row_domain([], _).
check_row_domain([H | T], N) :-
    between(1, N, H),
    check_row_domain(T, N).

check_row_unique(X) :-
    sort(X, XS),
    length(X, XL),
    length(XS, XSL),
    (XSL == XL).

check_each_row([]).
check_each_row([H|T]) :-
    check_row_unique(H),
    check_each_row(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
len_row(X, N) :- 
    length(X, N).

len_col([], _).
len_col([H|T], N) :-
    length(H,N),
    len_col(T, N).


transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).


kenken_testcase2(
    4,
  [
     +(6, [[1|1], [1|2], [2|1]]),
     *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
   -(1, [3|1], [3|2]),
   -(1, [4|1], [4|2]),%,
    +(8, [[3|3], [4|3], [4|4]]),
     *(2, [[3|4]])
  ]
).

% Both Tests were run on a 4X$ grid

%Timing of the plain_kenken:
% Times              since start      since last

%    user   time       8.356 sec       0.322 sec
%    system time       0.037 sec       0.016 sec
%    cpu    time       8.393 sec       4.961 sec
%    real   time     875.499 sec     245.535 sec

%timing of kenken using finite domain solver
% Times              since start      since last

%    user   time       8.359 sec       0.003 sec
%    system time       0.042 sec       0.005 sec
%    cpu    time       8.401 sec       0.008 sec
%    real   time     971.278 sec      95.779 sec

% as we can see, the pruning and search optimization of the fd solver kenken
% produces results much faster than the plain_kenken


% NO-OP KENKEN: 

%     we need to pass in more arguments as the grid does not have its operations 
%     set. 

%     noop_kenken(N, CN, T, CR)

%     N is the same as it was in the other kenkens: a number representing the size of the grid 

%     CN: Since we arent sure what the constraint operation is, this is a list of constraints that 
%         say what the desired value of the selected R|C pairs in the array should equate to 
%         format : (DESIRED_VALUE, LIST_OF_RC_PAIRS)
    
%     T: Just like in the previous kenkens, this represents our grid that may or may not be filled in 
%         or partially filled in 

%     CR: This keeps track of any constraints we discover or pre impose. So it is almost identical to CN, 
%         but it says which arithmetic operation each of the constraints in CN maps to. This has the same format 
%         as C in the previous Kenkens. (+,-,/,*)(VALUE, LIST)

    
%     IMPLEMENTATION: 

%         a lot of the functionality is similar to the previous kenkens. First we would validate the board in the 
%         exact same way i.e. we would make sure each row has numbers between 1-N, and each row consists of 
%         only unique numbers. 

%         A simple approach to validating the solution would be for each constraint in CN, we try each operator.
%         So, if we have a constraint C1: we would search the grid as if it were a + constraint and see if we can see a 
%         solution, if this fails we would backtrack and try -, /, * until we find a valid solution. Some obvious improvements 
%         would be to rule out certain scenarios. I.E. if our constraint is a list of more than 2 numbers, we know 
%         that it can only be +, or *. 

%         This does increase our runtime complexity a lot as for each constraint we have to try 4 possibilities. 


%         So an example of how it would run is it would take the head constraint, and then it would try to validate 
%         +head, and then validate the rest of the constraints with that imposed condition. this would repeat for each 
%         condition and if it fails, it would backtrack and try *head and would then search back through the rest of 
%         the constraints trying to match them accordingly. 

%         Once we find a constraint list that is valid for the grid we are done. 