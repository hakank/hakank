% https://open.kattis.com/problems/qaly
% Time limit: 1s
% Difficulty: 1.3 Easy

main :-
    read_int(_),
    read_two_floats(Total),
    writeln(Total),
    nl.
main.

read_two_floats(Total) :-
    read_line_to_string(user_input,Line),    
    read_two_floats(Line,0,Total).

read_two_floats(end_of_file,Total,Total).
read_two_floats(Line,Total0,Total) :-    
    split_string(Line, " ", "", [S1,S2]),
    number_string(N1,S1),
    number_string(N2,S2),
    read_line_to_string(user_input,Line2),
    Total1 is Total0 + N1*N2,
    read_two_floats(Line2,Total1,Total).

read_int(Num) :-
    read_line_to_string(user_input,Input),
    number_string(Num,Input).
