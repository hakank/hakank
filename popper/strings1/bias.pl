max_vars(3).
max_body(3).
max_clauses(3).

enable_recursion.
enable_pi.

% head
head_pred(f,2).
body_pred(f,2).
% copy1([H|RestIn]/[H|RestOut],[H|RestIn]/RestOut).
body_pred(copy1,2).
% skip1([_|RestIn]/Out,RestIn/Out).
body_pred(skip1,2).

type(f,(list,list)).
type(f,(list,list)).
type(copy1,(list,list)).
type(skip1,(list,list)).

% direction(f,(in,out)).
% direction(f,(in,out)).
% direction(copy1,(in,out)).
% direction(skip1,(in,out)).
