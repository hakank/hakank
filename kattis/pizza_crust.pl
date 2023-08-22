% https://open.kattis.com/problems/pizza2
% 1s
% 1.7 Easy

/*
Used Mathematica to simplify
  100*((R-C)/R)^2
to
  100*(1-C/R)^2

and saved two chars.

*/  

main :-
    readln([R,C]),
    format("~f",100*(1-C/R)^2).

/*
% Compressed: 47 chars Top 10 place 3
main:-readln([R,C]),format("~f",100*(1-C/R)^2).

*/

/*
main :-
    readln([R,C]),
    format("~f",100*((R-C)/R)^2).
*/

/*
% Compressed: 49 chars Top 10 place 3
main:-readln([R,C]),format("~f",100*((R-C)/R)^2).

*/

/*
main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,[R,C],Ss),
    X is 100*((R-C)^2/R^2),
    format("~7f~n",X).
*/