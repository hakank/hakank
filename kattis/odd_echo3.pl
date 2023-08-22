% https://open.kattis.com/problems/oddecho
% CPU time limit: 1s
% Difficulty: 1.3-1.4 Easy

% Shorter with read_string/3.

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(1,Ss).
s(_,[]).
s(I,[S|Ss]) :-
    (I mod 2 =:= 1 -> format("~s~n",[S]) ; true ),
    I1 is I+1,
    s(I1,Ss).
