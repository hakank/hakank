% https://open.kattis.com/problems/twosum
% 1s
% 1.4 Easy

main :-
    readln([A,B]),
    format("~d",[A+B]).

/*
% Compressed: 39 chars
main:-readln([A,B]),format("~d",[A+B]).

*/

/*
main :-
    read_line_to_string(user_input,S),
    split_string(S," ", "", Ss),
    maplist(number_string,[A,B],Ss),
    Sum is A + B,
    writeln(Sum).
main.
*/