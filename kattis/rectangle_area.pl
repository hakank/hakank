% https://open.kattis.com/problems/rectanglearea
% 1s 2048Mb
% 1.6 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[X1,Y1,X2,Y2],Ss),
    A is abs(X1-X2) * abs(Y1-Y2),
    format("~5f~n",[A]).
