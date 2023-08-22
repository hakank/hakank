% https://open.kattis.com/problems/chanukah
% 1s
% 1.4 Easy

% Still too slow for chanukah3.inp
main :-
    read_string(_),
    read_all(S),
    maplist(format('~w ~w~n'),S).
main.

read_all(S) :-
    read_string(In),
    read_all(In,[],S).
read_all(end_of_file,S,S).
read_all(In,S0,[[L,Sum]|S]) :-
    split_string(In," ","",[L,NS]),
    atom_number(NS,N),
    sum_all(1,N,N,Sum),
    read_string(S2),    
    read_all(S2,S0,S).

read_string(S) :-
    read_line_to_string(user_input,S).

sum_all(N,N,S,S2) :- S2 is S + N.
sum_all(I,N,S0,S) :-
    S1 is S0+I,
    I1 is I+1,
    sum_all(I1,N,S1,S).


