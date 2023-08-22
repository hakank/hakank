% https://open.kattis.com/problems/echoechoecho
% Time limit: 1s
% Diff 1.3 Easy

% 56 chars
main :-
    readln([S]),
    format('~s ~s ~s',[S,S,S]).

/*
% 45 chars Top list is 10..11 chars
main:-readln([S]),format('~s ~s ~s',[S,S,S]).
*/

/*
% 86 chars
main :-
    read_line_to_string(user_input,S),
    format('~s ~s ~s~n',[S,S,S]).
main.
*/