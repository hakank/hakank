max_vars(4).
max_body(3).
max_clauses(4).

enable_pi.
enable_recursion.

head_pred(cyclic,1).
body_pred(cyclic,1).
body_pred(arc,3).
body_pred(node,1).
%%body_pred(path,3). %% This is what we try to invent...

% type(cyclic,(graph,)).
% type(cyclic,(graph,1)).
% type(arc,(graph,node_type,node_type)).
% type(node,(node_type,)).
%% type(path,(graph,node_type,node)).

% :- modeh(1,cyclic(+graph))?
% :- modeb(1,cyclic(+graph))?
% :- modeb(*,arc(+graph,-node,-node))?
% :- modeb(*,path(+graph,+node,-node))?
