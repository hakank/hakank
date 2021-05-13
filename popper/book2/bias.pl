max_vars(8).
max_body(8).
max_clauses(1).

%% head
% head_pred(book,2).
% type(book,(id,list)).
% direction(book,(in,out)).

% head_pred(book,3).
% type(book,(element,element,element)).
% direction(book,(in,in,in)).

head_pred(book,4).
type(book,(id,element,element,element)).
direction(book,(in,out,out,out)).


% body
body_pred(author,2).
type(author,(id,element)).
direction(author,(in,out)).

body_pred(title,2).
type(title,(id,element)).
direction(title,(in,out)).

body_pred(editor,2).
type(editor,(id,element)).
direction(editor,(in,out)).

% body_pred(id,1).
% type(id,(integer,)).
% direction(id,(out,)).

% % extra
% body_pred(tail,2).
% type(tail,(list,list)).
% direction(tail,(in,out)).

% body_pred(head,2).
% type(head,(list,element)).
% direction(head,(in,out)).

% body_pred(last,2).
% type(last,(list,element)).
% direction(last,(in,out)).

% body_pred(length,2).
% type(length,(list,integer)).
% direction(length,(in,out)).

% body_pred(cons,3).
% type(cons,(element,list,list)).
% direction(cons,(in,in,out)).

% body_pred(append,3).
% type(append,(list,list,list)).
% direction(cons,(in,in,out)).

% body_pred(nth1,3).
% type(nth1,(integer,list,element)).
% direction(nth1,(in,in,out)).

% body_pred(empty,1).
% type(empty,(list,)).
% direction(empty,(in,)).

% body_pred(not_empty,1).
% type(not_empty,(list,)).
% direction(not_empty,(in,)).


% body_pred(c1,1).
% type(c1,(id,)).
% direction(c1,(in,)).

% body_pred(c2,1).
% type(c2,(id,)).
% direction(c2,(in,)).

% body_pred(c3,1).
% type(c3,(id,)).
% direction(c3,(in,)).


% body_pred(n1,3).
% type(n1,(integer,list,element)).
% direction(n1,(in,in,out)).

% body_pred(n2,3).
% type(n2,(integer,list,element)).
% direction(n2,(in,in,out)).

% body_pred(n3,3).
% type(n3,(integer,list,element)).
% direction(n3,(in,in,out)).

