% https://open.kattis.com/problems/twostones
% CPU time limit: 1s
% Difficulty: 1.3 Easy
% :- [kattio].
main1 :-
    read_int(N),
    (N mod 2 =:= 0 ->
        writeln("Bob")
    ;
        writeln("Alice")
    ).
main1.

% Shorter
main2 :-
    read_line_to_string(user_input,In),
    number_string(N,In),
    (N mod 2 =:= 0 ->
        writeln("Bob")
    ;
        writeln("Alice")
    ).
main2.


main :-
    read_string(user_input,In),
    number_string(N,In),
    (N mod 2 =:= 0 ->
        writeln("Bob")
    ;
        writeln("Alice")
    ).
main.
