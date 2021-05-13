max_vars(5).
max_body(3).
max_clauses(2).

%% Sum the elements of the list. In Haskell: foldl (+) 0 
%%
%% f(A,N):-
%%     empty(A), 
%%     zero(N).
%% f(A,N):-
%%     cons(HA, BA, A),
%%     f(BA, N2),
%%     sum(N2, HA, N).

% hakank: commented this
% Prevent recursion in first clause.
% :-
%     head_pred(P,A),
%     body_literal(0,_,P,A).

head_pred(f,2).
type(f,(list,int)).
direction(f,(in,out)).

body_pred(f,2).
%type(f,(list,int)).
%direction(f,(in,out)).

body_pred(empty,1).
type(empty,(list,)).
direction(empty,(in,)).

body_pred(zero,1).
type(zero,(int,)).
%%%% used to be out as direction
direction(zero,(out,)).

body_pred(sum,3).
type(sum,(int,int,int)).
direction(sum,(in,in,out)).

body_pred(cons,3).
type(cons,(int,list,list)).
direction(cons,(out,out,in)).

% #show var/4.
% #show literal/4.
