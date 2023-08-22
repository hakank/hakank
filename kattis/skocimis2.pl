% https://open.kattis.com/problems/skocimis
% 1s
% 1.6 Easy

main :-
    readln([A,B,C]),
    format("~d",[max(B-A,C-B)-1]).

/* 
% Compressed: 52 chars Top 10 place 8! 
main:-readln([A,B,C]),format("~d",[max(B-A,C-B)-1]).
*/



/*
% Compressed: 
main:-read_line_to_codes(user_input,S),split_string(S," ","",Ss),maplist(number_string,[A,B,C],Ss), M is max(B-A,C-B)-1,writeln(M). main.
*/

