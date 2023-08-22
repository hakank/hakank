% https://open.kattis.com/problems/erase
% 1s
% 1.7 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[N0|L]),
    number_string(N,N0),
    maplist(string_codes,L,[A,B]),
    ( N mod 2 =:= 0 ->
        (A==B ->
            T="Deletion succeeded"
        ;
            T="Deletion failed"
        )
    ;
        (s(A,B) ->
            T="Deletion succeeded"
        ;
            T="Deletion failed"
        )
    ),
    writeln(T).


s([],[]).
s([A|As],[B|Bs]) :-
    (A == B ->
        fail
    ;
        s(As,Bs)
    ).
