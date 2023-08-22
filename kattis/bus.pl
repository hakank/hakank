% https://open.kattis.com/problems/bus
% 1s
% 1.6 Easy

% I first used the clpfd model s2/1 to get
% the domain, the solutions and the lengths which gave
% https://oeis.org/A000225 "2^N-1".

% :- use_module(library(clpfd)).

main :-
    read_string(user_input, 100000,S),
    split_string(S,"\n","\n",Ss),
    maplist(number_string,[_|Ns],Ss),
    maplist(s,Ns).
main.
s(N) :- R is 2^N-1,writeln(R).


/*
s2(Len) :-
    N in 0..10000000000,
    N mod 2 #= 1,
    length(M,Len),
    t(N,[],M),
    writeln(m=M),
    reverse(M,MRev),
    writeln(MRev),
    label([N]),
    writeln(N).

t(N,L,L) :- N #= 0.
t(N,S0,[N1|S]) :-
    N #> 0,
    N1 #= (N div 2) + (1 div 2),
    N1 mod 2 #= 1 #\/ N1 #= 0,
    t(N1,S0,S).
*/