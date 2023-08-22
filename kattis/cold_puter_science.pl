% https://open.kattis.com/problems/cold
% CPU Time Limit: 1s
% Difficulty: 1.3 Easy

:- [kattio].

main :-
    read_int(_N),
    collect_lines_int(Lines),
    less_than_zero(Lines,Num),
    writeln(Num),
    nl.
main.


less_than_zero(L,N) :-
    less_than_zero(L,0,N).

less_than_zero([],N,N).
less_than_zero([T|Ts],N0,N) :-
    (T < 0 ->
        N1 is N0 +1
    ;
        N1 is N0
    ),
    less_than_zero(Ts,N1,N).

read_string(S) :-
    read_line_to_string(user_input,S).


collect_lines_int(Lines) :-
    read_int(X),
    collect_lines_int(X,[],Lines).

collect_lines_int(X,Lines,Lines) :-
    X == end_of_file, !.
            
collect_lines_int(X,Lines0,[X|Lines]) :-
    read_int(X2),
    collect_lines_int(X2,Lines0,Lines).

