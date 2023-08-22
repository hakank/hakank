% https://open.kattis.com/problems/romans
% 1s
% 1.6 Easy

% 81 chars
main:-
    readln([S],end_of_file),
    format("~d~n",[round(S*1000*5280/4854)]).

/*
% Compressed: 71 chars  Top 10 is 24.38 chars
main:-readln([S],end_of_file),format("~d~n",[round(S*1000*5280/4854)]).

*/
    