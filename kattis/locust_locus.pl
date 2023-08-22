% https://open.kattis.com/problems/locustlocus
% 1s
% 2.0 Easy

main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    s(Ns,[],T),
    min_list(T,Min),
    writeln(Min).

s([],L,L).
s([Y,C1,C2|Ss],L0,[V|L]) :-
    V is Y+lcm(C1,C2),
    s(Ss,L0,L).
    
    

