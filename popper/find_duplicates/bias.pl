max_vars(5).
max_body(6).
max_clauses(2).

head_pred(f,2).
type(f,(list,element)).
direction(f,(in,out)).

body_pred(f,2).
type(f,(list,element)).
direction(f,(in,out)).

body_pred(tail,2).
type(tail,(list,list)).
direction(tail,(in,out)).

body_pred(head,2).
type(head,(list,element)).
direction(head,(in,out)).

body_pred(empty,1).
type(empty,(list,)).
direction(empty,(in,)).


body_pred(last,2).
type(last,(list,element)).
direction(last,(in,out)).

body_pred(element,2).
type(element,(list,element)).
direction(element,(in,out)).
