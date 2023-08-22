% https://open.kattis.com/problems/pot
% 1s
% 1.4 Easy

main :-
    read_int(_),
    read_all_int(All),
    format('~d~n',[All]).
main.

read_all_int(All) :-
    read_string(In),
    read_all(In,0,All).

read_all(end_of_file,S,S).
read_all(In,S0,S) :-
    atom_chars(In,Cs),
    length(Last,1),
    append(First,Last,Cs),
    number_chars(FirstN,First),
    number_chars(LastN,Last),  
    T is FirstN^LastN,
    S1 is S0+T,
    read_string(S2),   
    read_all(S2,S1,S).

read_int(N) :-
    read_string(In),
    number_string(N,In).

read_string(S) :-
    read_line_to_string(user_input,S).
