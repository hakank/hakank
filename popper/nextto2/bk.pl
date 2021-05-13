:- use_module(library(clpfd)).

nth1x(Ix,List,E) :-
        nth1(Ix,List,E).

diff1(X,Y) :-
        Y - X #= 1. 

diff(X,Y,Diff) :-
        Y - X #= Diff. 

const1(1).
