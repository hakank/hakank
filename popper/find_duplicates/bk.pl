head([H|_],H).
tail([_|T],T).
last([A],A):-!.
last([_|T],A):-
  last(T,A).

element([X|_T],X).
element([_|T],X):-
    element(T,X).

empty([]).