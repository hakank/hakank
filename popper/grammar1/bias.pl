max_vars(8).
max_body(8).
max_clauses(2).

head_pred(s,2).
body_pred(det,2).
body_pred(np,2).
body_pred(vp,2).
body_pred(prep,2).
body_pred(noun,2).
body_pred(verb,2).

% modeh(1,s(+wlist,-wlist))?
% modeb(1,det(+wlist,-wlist))?
% modeb(*,np(+wlist,-wlist))?
% modeb(*,vp(+wlist,-wlist))?
% modeb(1,prep(+wlist,-wlist))?
% modeb(1,noun(+wlist,-wlist))?
% modeb(1,verb(+wlist,-wlist))?
