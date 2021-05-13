

head_tail([H|T],H,T).
empty([]).
not_empty([_]).
not_empty([_|-]).

head([H|_],H).
tail([_|T],T).