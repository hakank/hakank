kommentar(X) :- ('from the same article as log.pro').

path(a,b).
path(b,c).
path(c,d).
path(f,c).
path(b,e).
path(d,e).
path(e,f).

route(X , X , T ).
route(X , Y , T ) :- path(X,Z),
                     not(member(Z, T)),
                     route(Z,Y,[Z|T]).


member(X, [X|Y]).
member(X, [H|L]) :- member(X,L).



