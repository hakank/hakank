% https://open.kattis.com/problems/findingana
% Time limit: 1s
% Diff 1.3 Easy

main :-
    read_line_to_codes(user_input,C),
    append(_,[97|R],C),
    format('a~s~n',[R]).

/*
% Compressed: 78 chars
main:-read_line_to_codes(user_input,C),append(_,[97|R],C),format('a~s~n',[R]).

*/
