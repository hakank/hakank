% https://open.kattis.com/problems/carrots
% CPU time limit: 1s
% Difficulty: 1.4 Easy

main :-
    readln([_,B]),
    writeln(B).

/*
% Compressed: 31 chars Not short enough for Top 10 (13..15 chars)
main:-readln([_,B]),writeln(B).
*/


/*
main :-
    read_line_to_string(user_input,Line),
    split_string(Line," ","", [_,C]),
    writeln(C).
main.
*/