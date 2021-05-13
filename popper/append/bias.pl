max_vars(4).
max_body(2).
max_clauses(2).

% enable_pi.
enable_recursion.

head_pred(target,3).
body_pred(target,3).
% body_pred(head,2).
body_pred(tail,2).
% body_pred(head_tail,3).
body_pred(empty,1).
% body_pred(not_empty,1).
% body_pred(to_list,2).
body_pred(eq,2).
% body_pred(member,2).
% body_pred(append,3). % TEST
% body_pred(cons,3). % TEST
% body_pred(conc,3). % TEST

type(target,(list,list,list)).
type(head,(list,element)).
type(tail,(list,list)).
type(head_tail,(list,element,list)).
type(empty,(list,)).
type(not_empty,(list,)).
type(to_list,(element,list)).
type(eq,(list,list)).
type(cons,(element,list,list)).
type(conc,(list,list,list)).


% direction(target,(in,in,out)).
% direction(head,(in,out)).
% direction(tail,(in,out)).
% direction(head_tail,(in,out,out)).
% direction(empty,(in,)).
% direction(not_empty,(in,)).
% direction(to_list,(in,out)).
% direction(eq,(in,out)).

