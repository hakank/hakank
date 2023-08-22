% https://open.kattis.com/problems/herman
% 1s
% 1.7 Easy

% Python3: 63 chars (Top 10 is 28..55 chars)
% import math;r=int(input());print(f"{math.pi*(r**2)}\n{2*r**2}")
% import math;r=int(input());p=r**2;print(f"{math.pi*p}\n{2*p}")
% 57 chars:
% p=3.1415926;r=int(input());v=r**2;print(p*v,2*v,sep="\n")

% Using readln/2 instead: 65 chars (54 chars compressed)
main :-
    readln([S]),
    format("~6f~n~6f~n",[pi*S^2,2*S^2]).

/*
% Comparessed: 54 chars
main:-readln([S]),format("~6f~n~6f~n",[pi*S^2,2*S^2]).
*/

% 111 chars
/*
main :-
    read_line_to_string(user_input,S),
    number_string(R,S),
    format("~6f~n~6f~n",[pi*R^2,2*R^2]).

% Compressed: 95 chars (Andriy Zyevakov has 94 chars)
% main:-read_line_to_string(user_input,S),number_string(R,S),format("~6f~n~6f~n",[pi*R^2,2*R^2]).

*/