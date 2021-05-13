max_vars(4).
max_body(3).
max_clauses(2).

% head
head_pred(find_last,2).
type(find_last,(list,element)).
direction(find_last,(in,out)).

% body
body_pred(find_last,2).
type(find_last,(list,element)).
direction(find_last,(in,out)).

% my_head
body_pred(my_head,2).
type(my_head,(list,element)).
direction(my_head,(in,out)).

% my_tail
body_pred(my_tail,2).
type(my_tail,(list,list)).
direction(my_tail,(in,out)).

% my_length
% body_pred(my_length,2).
% type(my_length,0,list).
% type(my_length,1,element).
% direction(my_length,0,in).
% direction(my_length,1,out).


%% my_reverse: Using this makes it too simple. :-)
body_pred(my_reverse,2).
type(my_reverse,(list,list)).
direction(my_reverse,(in,out)).

% empty
body_pred(empty,1).
type(empty,(list,)).
direction(empty,(in,)).

% not_empty
body_pred(not_empty,1).
type(not_empty,(list,)).
direction(not_empty,(in,)).
