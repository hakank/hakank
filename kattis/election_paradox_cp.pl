% https://open.kattis.com/problems/electionparadox
% 1s
% 1.9 Easy

% Assumptions:
% A gets the most individual votes
% B get the more majority states than A

% Trying w/

:- use_module(library(clpfd)).
main :-
    read_string(user_input, 1000000,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    sum_list(Ns,NSum),
    length(Ns,Len),
    length(A,Len),
    length(B,Len),
    length(M,Len),
    s(Ns,A,B,M),
    sum(A,#=,ASum),
    sum(B,#=,BSum),
    ASum + BSum #= NSum,
    ASum #> BSum,
    
    sum(M,#=,MSum),
    Len2 is Len // 2,
    MSum #> Len2,    
    append([A,B,M],Vars),
    labeling([max(ASum)],Vars),
    writeln(a=A),
    writeln(b=B),
    writeln(m=M),
    writeln(asum=ASum),
    writeln(bsum=BSum),
    writeln(msum=MSum),
    nl.


s([],[],[],[]).
s([N|Ns],[A|As],[B|Bs],[M|Ms]) :-
    A in 0..N,
    B in 0..N,
    M in 0..1,
    A + B #= N,
    B #> A #<==> M#=1,
    s(Ns,As,Bs,Ms).
