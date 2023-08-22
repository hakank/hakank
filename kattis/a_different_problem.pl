% Problem https://open.kattis.com/problems/different
% Solution from https://open.kattis.com/help/prolog
% CPU time limit: 1s
% Difficulty: 2.6 Easy

% :- [kattio].

main :-
    repeat,
    read_int(X), read_int(Y),
    (X == end_of_file ;
     solve(X, Y),
     fail
    ).

solve(X, Y) :-
    Z is abs(X-Y),
    write(Z), nl.

read_int(N) :-
    read_string(In),
    number_string(N,In).
