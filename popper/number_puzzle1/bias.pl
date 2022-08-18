% max_vars(8).
% max_body(8).
% max_rules(4).

allow_singletons.
% non_datalog.

head_pred(f,3).

% body_pred(f,3).
body_pred(plus,3).
body_pred(minus,3).
body_pred(mult,3).
body_pred(div,3).

direction(f,(in,in,out)).
direction(plus,(in,in,out)).
direction(minus,(in,in,out)).
direction(mult,(in,in,out)).
direction(div,(in,in,out)).
