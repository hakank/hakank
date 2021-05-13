max_vars(6).
max_body(4).
max_clauses(4).

enable_pi.
enable_recursion.

%% First solution found:
%%   mult(B,A,D),plus(D,A,C)
%% Skipping this:
% :-
%         body_literal(0,mult,3,(1,0,3)),
%         body_literal(0,plus,3,(3,0,2)).

%% Second solution
%%    minus(C,A,D),mult(A,B,D).
%% Skipping this:
% :-
%         body_literal(0,minus,3,(2,0,3)),
%         body_literal(0,mult,3,(0,1,3)).

% General skip
% :-
%         body_literal(0,mult,3,_),
%         body_literal(0,plus,3,_).

% :-
%         body_literal(0,mult,3,_),
%         body_literal(0,minus,3,_).

head_pred(f,3).
body_pred(f,3).
body_pred(plus,3).
body_pred(minus,3).
body_pred(mult,3).
body_pred(div,3).

direction(f,(in,in,out)).
direction(plus,(in,in,out)).
direction(minus,(in,in,out)).
direction(mult,(in,in,out)).
direction(div,(in,in,out)).