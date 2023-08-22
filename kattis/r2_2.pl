% https://open.kattis.com/problems/r2
% CPU time limit: 1s
% Difficulty: 1.3 Easy

main :-
    readln([R1,S],end_of_file),
    format("~d",[S*2-R1]).

/*
% Compressed: 55 chars
main:-readln([R1,S],end_of_file),format("~d",[S*2-R1]).
*/

/*
main :-
    read_line_to_string(user_input,Str),
    split_string(Str," ", "", Ss),
    maplist(number_string,[R1,S],Ss),
    R2 is S*2-R1,
    writeln(R2).
main.
*/
