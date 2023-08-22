% https://open.kattis.com/problems/greetings2
% Time: 1s
% Difficulty: 1.3 Easy

main :-
    read_line_to_codes(user_input,S),
    append([104|Es],[121],S),
    append([104,Es|Es],[121],S2),
    flatten(S2,Out),
    format('~s~n',[Out]).
main.