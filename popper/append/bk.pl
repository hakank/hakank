% clist([]).
% clist([H|T]) :- constant(H), clist(T).

head([H|_],H).
tail([_|T],T).
head_tail([H|T],H,T).

cons(H,T,[H|T]).

empty([]).
not_empty([_]).
not_empty([_|_]).

to_list(X,[X]).

conc(A,B,C) :-
        append(A,B,C).

eq(X,X).