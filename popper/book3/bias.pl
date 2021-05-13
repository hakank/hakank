max_vars(5).
max_body(4).
max_clauses(1).

% enable_pi.
% enable_recursion.

head_pred(book,2).
body_pred(author,2).
body_pred(title,2).
body_pred(editor,2).
body_pred(list3,4).

type(book,(id,list)).
type(author,(id,element)).
type(title,(id,element)).
type(editor,(id,element)).
type(list3,(list,element,element,element)).

direction(book,(in,in)).
direction(author,(out,out)).
direction(title,(out,out)).
direction(editor,(out,out)).
direction(list3,(in,out,out,out)).
