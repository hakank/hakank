%% background knowledge

my_length(A,B):-
    is_list(A),
    (nonvar(B)->integer(B);true),
    length(A,B).

my_head([H|_],H).

my_tail([_|T],T).

my_reverse(A,B):-
        reverse(A,B).

empty([]).

not_empty([_|_]).
