max_vars(6).
max_body(8).
max_clauses(2).

head_pred(f,2).
type(f,(list,integer)).
direction(f,(in,out)).

body_pred(f,2).

body_pred(head,2).
type(head,(list,element)).
direction(head,(in,out)).

body_pred(tail,2).
type(tail,(list,list)).
direction(tail,(in,out)).

body_pred(succ,2).
type(succ,(integer,integer)).
direction(succ,(in,out)).

body_pred(empty,1).
type(empty,(list,)).
direction(empty,(in,)).

body_pred(zero,1).
type(zero,(integer,)).
direction(zero,(out,)).

body_pred(one,1).
type(one,(integer,)).
direction(one,(out,)).