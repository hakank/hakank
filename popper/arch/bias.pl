max_vars(5).
max_body(8).
max_clauses(2).

enable_pi.
enable_recursion.

head_pred(arch,3).
body_pred(left_of,2).
body_pred(supports,2).
body_pred(touches,2).
body_pred(brick,1).
body_pred(wedge,1).
body_pred(block,1).
body_pred(not_touches,2).

% type(arch,(block_type,block_type,block_type)).
% type(left_of,(block_type,block_type)).
% type(supports,(block_type,block_type)).
% type(touches,(block_type,block_type)).
% type(not_touches,(block_type,block_type)).
% type(brick,(block_type,)).
% type(wedge,(block_type,)).
% type(block,(block_type,)).

% direction(arch,(in,in,out)).
% direction(left_of,(in,out)).
% direction(supports,(in,out)).
% direction(touches,(in,out)).
% direction(not_touches,(in,out)).
% direction(brick,(in,)).
% direction(wedge,(in,)).
% direction(block,(in,)).

