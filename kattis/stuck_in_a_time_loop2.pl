% https://open.kattis.com/problems/timeloop
% CPU Time limit: 1s
% Difficulty: 1.3 Easy

main :-
    read_line_to_string(user_input,In),
    number_string(N,In),
    a(0,N).
main.

a(N,N).
a(C,N) :-
    C1 is C+1,    
    format('~d Abracadabra~n',[C1]),
    a(C1,N).
