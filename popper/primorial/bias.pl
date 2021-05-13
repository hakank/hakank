max_vars(16).
max_body(16).
max_clauses(15).

enable_pi.
enable_recursion.

head_pred(primorial,1).
body_pred(primorial,1).
body_pred(div2,1).
body_pred(div3,1).
body_pred(div5,1).
body_pred(div7,1).
body_pred(div11,1).
body_pred(div13,1).

body_pred(const2,1).
body_pred(const3,1).
body_pred(const5,1).
body_pred(const7,1).
body_pred(const11,1).
body_pred(const13,1).
% body_pred(const17,1).
% body_pred(const19,1).
% body_pred(const23,1).
% body_pred(const29,1).


type(primorial,(element,)).
type(div2,(element,)).
type(div3,(element,)).
type(div5,(element,)).
type(div7,(element,)).
type(div11,(element,)).
type(div13,(element,)).
type(const2,(element,)).
type(const3,(element,)).
type(const5,(element,)).
type(const7,(element,)).
type(const11,(element,)).
type(const13,(element,)).

% direction(primorial,(in,)).
% direction(div2,(in,)).
% direction(div3,(in,)).
% direction(div5,(in,)).
% direction(div7,(in,)).
% direction(div11,(in,)).
% direction(div13,(in,)).

