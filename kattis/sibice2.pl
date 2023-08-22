% https://open.kattis.com/problems/sibice
% CPU Time limit: 1s
% Difficulty: 1.4 Easy

main :-
    read_string(S),
    split_string(S," ", "", S3),
    maplist(number_string,[_,W,H],S3),
    repeat,
    read_string(Len),
    (Len == end_of_file ;
        number_string(Len1,Len),
        (check_line(W,H,Len1) ->
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

read_string(S) :-
    read_line_to_string(user_input,S).
