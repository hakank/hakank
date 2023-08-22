% https://open.kattis.com/problems/sibice
% CPU Time limit: 1s
% Difficulty: 1.4 Easy

:- [kattio].

main :-
    read_int(_N),
    read_int(W),
    read_int(H),
    repeat,
    read_int(Len),
    (Len == end_of_file ;
        (check_line(W,H,Len) ->
            writeln("DA")
        ;
            writeln("NE")
        ),
        fail
    ).
main.


check_line(W,H,Len) :-
    T is sqrt(W^2+H^2),
    Len =< T.