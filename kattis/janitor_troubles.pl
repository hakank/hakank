% https://open.kattis.com/problems/janitortroubles
% 1s
% 1.4 Easy

% Port of Python code https://www.geeksforgeeks.org/maximum-area-quadrilateral/

main :- read_line_to_string(user_input,S),split_string(S," ","",Ss),
maplist(number_string,[A,B,C,D],Ss),
Semi is (A+B+C+D) / 2,
X is sqrt((Semi-A)*(Semi-B)*(Semi-C)*(Semi-D)),
format('~10f~n',[X]).
main.
    