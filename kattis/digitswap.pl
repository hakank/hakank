% https://open.kattis.com/problems/digitswap
% Time limit: 1s
% Diff 1.3 Easy

% 77 chars
main :-
    read_line_to_codes(user_input,[A,B]),
    format('~c~c~n',[B,A]).

/*
% Compressed: 66 chars
main:-read_line_to_codes(user_input,[A,B]),format('~c~c~n',[B,A]).
*/