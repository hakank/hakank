max_vars(6).
max_body(4).
max_clauses(1).

head_pred(nexttox,3).
body_pred(nexttox,3).
body_pred(diff1,2).
body_pred(diff,3).
body_pred(nth1x,3).
body_pred(const1,1).

type(nexttox,(element,element,list)).
type(diff1,(integer,integer)).
type(diff,(integer,integer,integer)).
type(nth1x,(integer,list,element)).
type(const1,(integer,)).
