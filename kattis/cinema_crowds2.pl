% https://open.kattis.com/problems/cinema2
% 1s
% 1.9 Easy

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[M,_|Ns],Ss),
    length(Ns,Len),
    s(Ns,M,0,0,R),
    V is Len - R,
    writeln(V).

s([],_,_,X,X).
s([N|Ns],Max,A0,C0,C) :-
    A1 is A0+N,
    (A1 =< Max -> C1 is C0+1 ; C1 is C0 ),
    s(Ns,Max,A1,C1,C).

