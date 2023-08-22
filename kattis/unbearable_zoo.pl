% https://open.kattis.com/problems/zoo
% 1s
% 1.9 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",Ss),
    s(1,Ss).
s(_,[]).
s(_,["0"]).
s(I,[S|Ss]) :-
    number_string(N,S),
    length(L,N),
    append(L1,Ss2,Ss),
    maplist(string_lower,L1,L2),
    maplist(ss,L2,L),
    msort(L,Sorted),
    clumped(Sorted,Cl),
    format("List ~d:~n",[I]),
    maplist(p,Cl),
    I1 is I + 1,
    s(I1,Ss2).
p(A-C) :- format("~s | ~d~n",[A,C]).
ss(S,Last) :- split_string(S," ","",Ss),last(Ss,Last).