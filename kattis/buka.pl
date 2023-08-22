% https://open.kattis.com/problems/buka
% 1s
% 2.0 Easy

% 89 chars
main:-
    read_string(user_input,_,S),
    read_from_chars(S,A),
    format("~d~n",[A]).

/*
% Compressed: 74 char: Not short enough for Top 10 (18..32)
main:-read_string(user_input,_,S),read_from_chars(S,A),format("~d~n",[A]).

*/