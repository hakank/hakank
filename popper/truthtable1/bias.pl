% Inspired by
% From https://www.youtube.com/watch?v=u-un2AHAphg&list=PL-qvsLbZq06IgkKk7zeUd9-7IjdjLGtwt&index=9
% Automated Synthesis of Boolean expressions from Truth Tables
% (define-fun f ((p Bool) (q Bool)) Bool (or p (not p)))

% Two solutions
% f(A,B):-ptrue(A).
% f(A,B):-pnot(B).

% Another solution
% f(A,B):-pand(C,A).
% f(A,B):-pfalse(B).


max_vars(4).
max_body(4).
max_clauses(4).

allow_singletons.
non_datalog.
% enable_pi.
% enable_recursion.


% head
head_pred(f,2).

% body
% body_pred(f,2).
body_pred(ptrue,1).
body_pred(pfalse,1).
body_pred(pand,2).
body_pred(por,2).
body_pred(pxor,2).
body_pred(pnot,1).

