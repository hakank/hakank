% https://open.kattis.com/problems/ladder
% 1s
% 1.6 Easy

main :-
    readln([L,D]),
    format("~d",[ceiling(L/sin(D*pi/180))]).

/*
% Compressed: 60 chars Top 10 place 4! Much because ceil/sin/pi require math module imports/calls
main:-readln([L,D]),format("~d",[ceiling(L/sin(D*pi/180))]).

*/

/*

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[L,D],Ss),
    X is ceiling(L/sin(D*pi/180)),
    writeln(X).
*/