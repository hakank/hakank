max_vars(6).
max_body(4).
max_clauses(4).

% head
head_pred(target,2).
type(target,(element,element)).
direction(target,(in,out)).


% body
body_pred(target,2).
type(target,(element,element)).
direction(target,(in,out)).


body_pred(succ,2).
type(succ,(element,element)).
direction(succ,(in,out)).

% Testing:

% Too easy :-)
% body_pred(lt,2).
% type(lt,0,element).
% type(lt,1,element).
% direction(lt,0,in).
% direction(lt,1,out).

% body_pred(le,2).
% type(le,0,element).
% type(le,1,element).
% direction(le,0,in).
% direction(le,1,out).

% body_pred(gt,2).
% type(gt,0,element).
% type(gt,1,element).
% direction(gt,0,in).
% direction(gt,1,out).

% body_pred(ge,2).
% type(ge,0,element).
% type(ge,1,element).
% direction(ge,0,in).
% direction(ge,1,out).



