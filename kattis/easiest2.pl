% https://open.kattis.com/problems/easiest
% 1s
% 1.6 Easy

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",Ss),
    maplist(number_string,Ns,Ss),
    p(Ns).
p([]).
p([0]).
p([N|Ns]) :-
    dsum(N,Sum),
    c(11,N,Sum,P),
    writeln(P),
    p(Ns).
c(I,N,Sum,P) :-
    NI is I*N,(dsum(NI,Sum)->P = I;I1 is I+1,c(I1,N,Sum,P) ).
dsum(X, X) :- X<10.
dsum(X, Y) :- X>=10, X1 is X // 10, X2 is X mod 10, dsum(X1, Y1), Y is Y1 + X2.
