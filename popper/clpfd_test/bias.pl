max_vars(6).
max_body(6).
max_clauses(2).

%% Note: One must change the head_pred depending on the 
%%structure of the problem.

%% ..../1
%% head
% head_pred(target,1).
% type(target,(list,)).
% direction(target,(in,)).

%% Testing latin_square, maplist3, as a matrix
% head_pred(target,1).
% type(target,(matrix,)).
% direction(target,(in,)).


% ..../2
% For alldifferent_modulo/2, min_list_clp/2, max_list_clp/2
%% head
% head_pred(target,2).
% type(target,(list,element)).
% direction(target,(in,out)).

% % body
% body_pred(target,2).
% type(target,(list,element)).
% direction(target,(in,out)).

% for "Two lists: one alldifferent and one increasing
% % head
% head_pred(target,2).
% type(target,(list,list)).
% direction(target,(in,in)).


% .../3
% For atmost/3, element3/3, exactly/3
% head (what should the directions be?)
head_pred(target,3).
type(target,(element,list,element)).
direction(target,(in,in,out)).


% .../3 for scalar_product2
% head_pred(target,3).
% type(target,(list,list,element)).
% direction(target,(in,in,out)).

% for increasing_with/3 
% This don't work as expected
% head_pred(target,3).
% type(target,(list,element,list)).
% direction(target,(in,in,out)).

%% body
body_pred(target,3).
% type(target,(element,list,element)).
% direction(target,(in,in,in)).


body_pred(my_tail,2).
type(my_tail,(list,list)).
direction(my_tail,(in,out)).


body_pred(my_head,2).
type(my_head,(list,element)).
direction(my_head,(in,out)).


body_pred(all_different,1).
type(all_different,(list,)).
direction(all_different,(in,)).


body_pred(alldifferent_except_0,1).
type(alldifferent_except_0,(list,)).
direction(alldifferent_except_0,(in,)).

body_pred(alldifferent_modulo,2).
type(alldifferent_modulo,(list,element)).
direction(alldifferent_modulo,(in,out)).


body_pred(all_distinct,1).
type(all_distinct,(list,)).
direction(all_distinct,(in,)).

body_pred(circuit,1).
type(circuit,(list,)).
direction(circuit,(in,)).


body_pred(increasing,1).
type(increasing,(list,)).
direction(increasing,(in,)).

% body_pred(increasing_with,3).
% type(increasing,(list,element,list)).
% direction(increasing,(in,in,out)).


% body_pred(maplist,2).
type(maplist,(function,list)).
direction(maplist,(in,out)).
% functional(maplist,2).

body_pred(maplist3,2).
type(maplist3,(function,matrix)).
direction(maplist3,(in,out)).
% functional(maplist3,2).

% Note the direction: (out,)
body_pred(function_all_different,1).
type(function_all_different,(function,)).
direction(function_all_different,(out,)).

body_pred(function_alldifferent_except_0,1).
type(function_alldifferent_except_0,(function,)).
direction(function_alldifferent_except_0,(out,)).

body_pred(function_circuit,1).
type(function_circuit,(function,)).
direction(function_circuit,(out,)).

body_pred(function_increasing,1).
type(function_increasing,(function,)).
direction(function_increasing,(out,)).

% body_pred(function_increasing_with,1).
% type(function_increasing_with,(function,)).
% direction(function_increasing_with,(out,)).


body_pred(function_inverse,1).
type(function_inverse,(function,)).
direction(function_inverse,(out,)).


body_pred(modulo,3).
type(modulo,(element,element,element)).
direction(modulo,(in,in,out)).

body_pred(min_list_clp,2).
type(min_list_clp,(list,element)).
direction(min_list_clp,(in,out)).

body_pred(max_list_clp,2).
type(max_list_clp,(list,element)).
direction(max_list_clp,(in,out)).

body_pred(to_num,2).
type(to_num,(list,element)).
direction(to_num,(in,out)).

body_pred(element3,3).
type(element3,(element,list,element)).
direction(element3,(in,in,out)).

body_pred(inverse,1).
type(inverse,(list,)).
direction(inverse,(in,)).

% TODO: Sudoku: latin_square2 + check of the cells

body_pred(latin_square2,1).
type(latin_square2,(list,)).
direction(latin_square2,(in,)).

% This works if target is of type matrix
% body_pred(latin_square,1).
% type(latin_square,(matrix,)).
% direction(latin_square,(in,)).


body_pred(is_square,2).
type(is_square,(element,element)).
direction(is_square,(in,out)).

body_pred(chunks_of,3).
type(chunks_of,(list,element,list_of_lists)).
direction(chunks_of,(in,in,in)).

body_pred(scalar_product2,3).
type(scalar_product2,(list,list,element)).
direction(scalar_product2,(out,out,out)).

% atmost(N,L,V)
%
% The value V in list L can have at most N occurrences.
% 
body_pred(atmost,3).
type(atmost,(element,list,element)).
direction(atmost,(in,in,out)).
% direction(atmost,(out,in,in)). % Testing

body_pred(exactly,3).
type(exactly,(element,list,element)).
direction(exactly,(in,in,out)).


% body_pred(transpose,2).
% type(transpose,(matrix,matrix)).
% direction(transpose,(in,out)).
% irreflexive(transpose,2).

body_pred(flatten,2).
type(flatten,(matrix,list)).
type(flatten,(in,out)).

body_pred(plus,3).
type(plus,(element,element,element)).
type(plus,(in,in,out)).

body_pred(times,3).
type(times,(element,element,element)).
type(times,(in,in,out)).

body_pred(minus,3).
type(minus,(element,element,element)).
type(minus,(in,in,out)).

% body_pred(divide,3).
% type(divide,(element,element,element)).
% type(divide,(in,in,out)).

