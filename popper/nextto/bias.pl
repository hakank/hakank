max_vars(4).
max_body(3).
max_clauses(2).

head_pred(nextto,3).
body_pred(nextto,3).
body_pred(head,2).
body_pred(tail,2).

type(nextto,(element,element,list)).
type(head,(list,element)).
