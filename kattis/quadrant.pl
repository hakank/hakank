% https://open.kattis.com/problems/quadrant
% Time limit: 1s
% Difficulty: 1.3 Easy

main :-
    read_int(X), read_int(Y),
    q(X,Y,Q),
    writeln(Q),
    nl.
main.

q(X,Y,1) :- X >= 0, Y >= 0.
q(X,Y,2) :- X =< 0, Y >= 0.
q(X,Y,3) :- X =< 0, Y =< 0.
q(X,Y,4) :- X >= 0, Y =< 0.

read_int(N) :-
    read_line_to_string(user_input,In),
    number_string(N,In).

