% https://open.kattis.com/problems/overdraft
% 1s
% 1.9 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",Ss),
    maplist(number_string,[_|Ns],Ss),
    s(Ns,[0],Asum),
    min_list(Asum,Min),
    T is 0-Min,
    writeln(T).

s([],S,S).
s([N|Ns],S0,S) :-
    last(S0,Last),
    M is Last+N,
    append(S0,[M],S1),
    s(Ns,S1,S).
