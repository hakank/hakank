% https://open.kattis.com/problems/hothike
% 2s
% 1.9 Easy

main :-
    read_string(user_input,_,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    length(Ns,Len),
    Max is Len*50,
    s(Ns,1,1,N,Max,M),
    format("~d ~d~n",[N,M]).
s([],_,D,D,M,M).
s([_],_,D,D,M,M).
s([_,_],_,D,D,M,M).
s([N1,N2,N3|Ns],I,D0,D,M0,M) :-
    T is max(N1,N3),(T < M0 -> M1 is T, D1 = I ; M1 = M0, D1 = D0 ),
    I1 is I+1,
    s([N2,N3|Ns],I1,D1,D,M1,M).
    
    