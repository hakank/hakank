max_vars(5).
max_body(6).
max_clauses(1).

%% add the last element to the head
%% f(A,B):-
%%     last(A,C),
%%     cons(C,A,B).

head_pred(f,2).
type(f,(list,list)).
direction(f,(in,out)).

body_pred(tail,2).
type(tail,(list,list)).
direction(tail,(in,out)).

body_pred(head,2).
type(head,(list,element)).
direction(head,(in,out)).

body_pred(last,2).
type(last,(list,element)).
direction(last,(in,out)).

body_pred(length,2).
type(length,(list,int)).
direction(length,(in,out)).

body_pred(sum,3).
type(sum,(int,int,int)).
direction(sum,(in,in,out)).

body_pred(cons,3).
type(cons,(element,list,list)).
direction(cons,(in,in,out)).

body_pred(empty,1).
type(empty,(list,)).
direction(empty,(in,)).

body_pred(zero,1).
type(zero,(int,)).
direction(zero,(in,)).

