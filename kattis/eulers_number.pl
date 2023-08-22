% https://open.kattis.com/problems/eulersnumber
% 1s
% 2.4 Easy

% Restrict to max 170 iterations (and only float operations), after
% that float overflow.
% Ok, that worked.

main :-
    read_line_to_string(user_input,S),
    number_string(N1,S),
    N is min(N1,170),
    e(1,N,1,0,E1),
    E is E1+1,
    writeln(E).

e(N1,N,_,E,E) :- N1 > N.
e(I,N,F0,E0,E) :-
    F1 is F0*I*1.0,    
    E1 is E0 + 1.0/F1,
    I1 is I+1,    
    e(I1,N,F1,E1,E).