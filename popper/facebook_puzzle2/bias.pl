max_vars(7).
max_body(2).
max_clauses(1).

allow_singletons.
non_datalog.

% enable_pi.
% enable_recursion.

%% Weed out symmetrical solutions.
% :-
%         body_literal(_,mult,3,(A,B,_)),
%         A > B.

% :-
%         body_literal(_,plus,3,(A,B,_)),
%         A > B.


head_pred(f,6).
% body_pred(f,6).
body_pred(plus,3).
% body_pred(plus3,4). 
% body_pred(minus,3).
% body_pred(mult,3).
% body_pred(div,3).
% body_pred(eq,2).
% body_pred(eq3,3).
% body_pred(not_eq,2).
% body_pred(nop,1).
% body_pred(nop2,2).
% body_pred(nop3,3).

direction(f,(in,in,in,in,in,out)).
direction(plus,(in,in,out)).
% direction(plus3,(in,in,in,out)).
% direction(minus,(in,in,out)).
% direction(mult,(in,in,out)).
% direction(div,(in,in,out)).
% direction(eq,(in,out)).
% direction(eq3,(in,in,in)).
% direction(not_eq,(in,out)).
% direction(nop,(in,)).


% functional(plus,3).
% functional(mult,3).

% irreflexive(plus,2).
% irreflexive(mult,2).
