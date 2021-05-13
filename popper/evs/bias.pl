max_vars(5).
max_body(7).
max_clauses(2).

%% TARGET
%%evs(A) :- empty(A).
%%evs(A) :- head(A,B),even(B),tail(A,C),head(C,D),odd(D),tail(C,E),evs(E).


head_pred(evs,1).
type(evs,(list,)).
direction(evs,(in,)).

body_pred(empty,1).
type(empty,(list,)).
direction(empty,(in,)).

body_pred(head,2).
type(head,(list,element)).
direction(head,(in,out)).

body_pred(even,1).
type(even,(element,)).
direction(even,(in,)).

body_pred(odd,1).
type(odd,(element,)).
direction(odd,(in,)).

body_pred(tail,2).
type(tail,(list,list)).
direction(tail,(in,out)).

body_pred(evs,1).
type(evs,(list,)).
direction(evs,(in,)). 
