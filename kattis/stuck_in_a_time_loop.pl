% https://open.kattis.com/problems/timeloop
% CPU Time limit: 1s
% Difficulty: 1.3 Easy

:- [kattio].

main :-
    read_int(N),
    write_abracadabra(0,N).
main.

write_abracadabra(N,N).
write_abracadabra(C,N) :-
    C1 is C+1,    
    format('~d Abracadabra~n',[C1]),
    write_abracadabra(C1,N).
