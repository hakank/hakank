% https://open.kattis.com/problems/areal
% 1s
% 1.5 Easy

main :-
    readln([N],end_of_file),
    format("~7f~n",[4*sqrt(N)]).

/*
% Compressed: 58 chars
main:-readln([N],end_of_file),format("~7f~n",[4*sqrt(N)]).

*/

/*
main :-
    read_line_to_string(user_input,S),
    number_string(A,S),
    L is 4*sqrt(A),
    format("~7f~n",[L]).
*/