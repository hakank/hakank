% https://open.kattis.com/problems/r2
% CPU time limit: 1s
% Difficulty: 1.3 Easy
:- [kattio].
main :-
    read_int(R1),read_int(S),
    R2 is S*2-R1,
    writeln(R2).
main.

