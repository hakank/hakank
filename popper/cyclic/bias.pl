max_vars(3).
max_body(2).
max_clauses(2).

% enable_pi.

head_pred(cyclic,1).
body_pred(cyclic,1).
body_pred(arc,3).
body_pred(path,3).

type(cyclic,(graph,)).
type(cyclic,(graph,1)).
type(arc,(graph,node,node)).
type(path,(graph,node,node)).

% :- modeh(1,cyclic(+graph))?
% :- modeb(1,cyclic(+graph))?
% :- modeb(*,arc(+graph,-node,-node))?
% :- modeb(*,path(+graph,+node,-node))?
