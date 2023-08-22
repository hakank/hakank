% https://open.kattis.com/problems/busskortet
% CPU time limit: 1s
% Difficulty 1.3-2.6 Easy

:- [kattio].
:- use_module(library(clpfd)).
main :-
    read_int(N),
    num_transactions(N,Diff,_Z),
    findall(Z,num_transactions(N,Diff,Z),All),
    sort(All,Sorted),
    [Min|_] = Sorted,
    writeln(Min),
    nl.
main.

num_transactions(N,Diff,Z) :-
    % L = [100,200,500],
    X = [X100,X200,X500],
    X ins 0..1000,
    S #= X100*100 + X200*200 + X500*500,
    S #>= N,
    Z #= X100+X200+X500,
    Diff #= abs(S-N),
    labeling([ff,bisect,min(Diff)], X).
    