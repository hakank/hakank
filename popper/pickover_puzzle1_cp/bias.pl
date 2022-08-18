% max_vars(8).
% max_body(8).
% max_clauses(4).

% enable_pi.
% enable_recursion.

% allow_singletons.
% non_datalog.


head_pred(f,4).

% body_pred(f,4).
body_pred(plus,3).
body_pred(minus,3).
body_pred(mult,3).
% body_pred(div,3).

% No solution when using direction.
% direction(f,(in,in,in,out)).

% direction(plus,(in,in,out)).
% direction(minus,(in,in,out)).
% direction(mult,(in,in,out)).
% direction(div,(in,in,out)).

% functional(plus,3).
% functional(minus,3).
% functional(mult,3).
% functional(div,3).

% irreflexive(plus,2).
% irreflexive(minus,2).
% irreflexive(mult,2).
% irreflexive(div,2).
