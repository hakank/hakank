max_vars(5).
max_body(6).
max_clauses(2).

%
% member(X,[X|_]).
% member(X,[_|L]) => member(X,L).
%

% head
head_pred(mem,2).
type(mem,(element,list)).
direction(mem,(out,in)). % note out, in


% body
body_pred(mem,2).
type(mem,(element,list)).
direction(mem,(out,in)).

body_pred(tail,2).
type(tail,(list,list)).
direction(tail,(in,out)).

body_pred(head,2).
type(head,(list,element)).
direction(head,(in,out)).

body_pred(empty,1).
type(empty,(list,)).
direction(empty,(in,)).
