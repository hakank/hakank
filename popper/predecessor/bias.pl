max_vars(4).
max_body(3).
max_clauses(2).

% head
head_pred(target,2).
type(target,(element,element)).
direction(target,(in,out)).

% body
body_pred(target,2).
type(target,(element,element)).
direction(target,(in,out)).

body_pred(succ,2).
type(succ,(element,element)).
direction(succ,(in,out)).
