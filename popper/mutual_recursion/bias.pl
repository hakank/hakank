max_vars(6).
max_body(3).
max_clauses(5).

enable_pi.
enable_recursion.

% head
head_pred(even,1).
body_pred(even,1).
body_pred(s,2).
body_pred(zero,1).
body_pred(one,1).
body_pred(eq,2).

type(even,(element,)).
type(s,(element,element)).


% direction(even,(in,)).
% direction(s,(in,out)).


% Testing: Yes!
% body_pred(odd,1).
% type(odd,0,element).
% direction(odd,0,in).


% Testing: Nope
% modeb(succ,2).
% type(succ,0,element).
% type(succ,1,element).
% direction(succ,0,in).
% direction(succ,1,out).

