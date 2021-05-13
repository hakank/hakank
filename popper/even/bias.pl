max_vars(3).
max_body(3).
max_clauses(2).

%% EVEN
%% f(A):-zero(A).
%% f(A):-succ(B,A),succ(C,B),f(C).

% Prevent recursion in first clause.
:-
    head_pred(P,A),
    body_literal(0,_,P,A).

head_pred(f,1).
type(f,(int,)).
direction(f,(in,)).

body_pred(f,1).

body_pred(zero,1).
type(zero,(int,)).
direction(zero,(in,)).

body_pred(succ,2).
type(succ,(int,int)).
direction(succ,(out,in)).
