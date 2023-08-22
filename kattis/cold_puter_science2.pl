% https://open.kattis.com/problems/cold
% CPU Time Limit: 1s
% Difficulty: 1.3 Easy

main :-
    read_string(_N),
    read_string(S),
    split_string(S," ","",Ss),
    maplist(number_string,Ns,Ss),
    less_than_zero(Ns,Num),
    writeln(Num).
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
