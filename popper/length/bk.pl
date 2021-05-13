

cons(A,B,C):-
        append([A],B,C).
tail([_|T],T).
head([H|_],H).
empty([]).

cons1(A,B,C):-
        cons(A,B,C).
cons2(A,B,C):-
        cons(A,B,C).
succ(A,B):-
        %% \+ is_list(A), % hakank
        %% \+ is_list(B), % hakank
        B is A+1.

is_list([]). % hakank
is_list([_|_]). % hakank

zero(0).
one(1).