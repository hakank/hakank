% https://open.kattis.com/problems/tarifa
% 1s
% 1.4 Easy

% Using read_string/3: 194 chars

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[Max,Num|Ns],Ss),
    sum_list(Ns,Sum),
    X is Max*Num - Sum + Max,
    writeln(X).
