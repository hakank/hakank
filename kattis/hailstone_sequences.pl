% https://open.kattis.com/problems/hailstone2
% 1s
% 1.8 Easy

main :-
    readln([N]),
    c(N,L),
    writeln(L).

:- table c/2.
c(1,1).
c(N,L) :-
    N>1,
    (N mod 2=:=0 ->
        T is N div 2
    ;
        T is 3*N+1
    ),
    c(T,L1),
    L is L1 + 1.

/*
% Compressed: 129 chars
main:-readln([N]),c(N,L), writeln(L).
:- table c/2.
c(1,1). c(N,L):-N>1,(N mod 2=:=0->T is N div 2;T is 3*N+1),c(T,L1),L is L1+1.
*/