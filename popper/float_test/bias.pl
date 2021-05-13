max_vars(4).
max_body(4).
max_clauses(4).

enable_pi.
% enable_recursion.

head_pred(target,3).
% body_pred(target,3).
body_pred(times,3).
% body_pred(division,3).
% body_pred(plus,3).
% body_pred(minus,3).

type(target,(float,float,float)).
type(times,(float,float,float)).
% % type(division,(float,float,float)).
% % type(plus,(float,float,float)).
% % type(minus,(float,float,float)).

% direction(target,(in,in,out)).
% direction(times,(in,in,out)).
% % direction(division,(in,in,out)).
% % direction(plus,(in,in,out)).
% % direction(minus,(in,in,out)).
