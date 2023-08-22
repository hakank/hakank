% https://open.kattis.com/problems/parking2
% Time limit: 1s
% Diff: 1.5 Easy

% Shorter with read_string/3.

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).
s([]).
s([_,S2|Ss]) :-
    split_string(S2," ","", T),
    maplist(number_string,Ns,T),
    min_list(Ns,Min),
    max_list(Ns,Max),
    Res is (Max-Min)*2,
    writeln(Res),
    s(Ss).
