% https://open.kattis.com/problems/romans
% 1s
% 1.6 Easy

main :-
    read_line_to_codes(user_input,S),
    number_string(N,S),
    X is round(N*1000*5280/4854),
    writeln(X).


/*
% 120 chars
main :-
    read_line_to_codes(user_input,S),
    number_string(N,S),
    X is round(N*1000*5280/4854),
    writeln(X).
*/

/*
% Compressed: 98 chars
main:-read_line_to_codes(user_input,S),number_string(N,S),X is round(N*1000*5280/4854),writeln(X).
*/
    