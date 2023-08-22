% https://open.kattis.com/problems/pervasiveheartmonitor
% 1s
% 1.8 Easy

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",Ss),
    s(Ss).
s([]).
s([S|Ss]) :-
    split_string(S," ","",L),
    findall(V,(member(T,L),(number_string(N,T) -> V = N ; V = T) ),Vs),
    partition(number,Vs,Nums,Str),
    avg(Nums,Avg),
    format('~6f ',[Avg]),
    maplist(format('~s '),Str),
    nl,
    s(Ss).

avg(L,A) :- length(L,Len), sum_list(L,Sum), A is Sum/Len.