% 
% From https://www.youtube.com/watch?v=u-un2AHAphg&list=PL-qvsLbZq06IgkKk7zeUd9-7IjdjLGtwt&index=9
% Automated Synthesis of Boolean expressions from Truth Tables
% (define-fun f ((p Bool) (q Bool) (r Bool)) Bool (or p (and q (not r))))
% 

% f(A,B,C):-ptrue(A).
% f(A,B,C):-pand(D,B),pnot(C).


% f(A,B,C):-pand(D,A).
% f(A,B,C):-pxor(B,C),pand(B,D).

% f(A,B,C):-ptrue(A).
% f(A,B,C):-por(B,C),pnot(C).


max_vars(4).
max_body(2).
max_clauses(2).

allow_singletons.
non_datalog.
% enable_pi.
% enable_recursion.


% head
head_pred(f,3).

% body
body_pred(ptrue,1).
body_pred(pfalse,1).
body_pred(pand,2).
body_pred(por,2).
% body_pred(pxor,2).
body_pred(pnot,1).

