max_vars(6).
max_body(3).
max_clauses(2).

% head
head_pred(target,1).
type(target,(element,)).
direction(target,(in,)).

% body
body_pred(target,1).
type(target,(element,)).
direction(target,(in,)).

body_pred(edge,2).
type(edge,(element,element)).
direction(edge,(in,out)).


body_pred(colour,2).
type(colour,(element,element)).
direction(colour,(in,out)).

