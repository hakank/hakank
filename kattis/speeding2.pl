% https://open.kattis.com/problems/speeding
% 1s
% 1.6 Easy

main :-
    read_string(user_input,10000,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(s,Ss,L),
    findall(Speed,(nextto([T1,V1],[T2,V2],L),Speed is floor((V2-V1)/(T2-T1))  ),Ps),
    max_list(Ps,Max),
    writeln(Max).
s(S,[T,V]) :- split_string(S," ","",TV),maplist(number_string,[T,V],TV).
