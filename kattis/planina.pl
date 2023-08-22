% https://open.kattis.com/problems/planina
% 1s
% 1.4 Easy

% Formula from https://oeis.org/A028400
% found after some googling of 9 25 1089

main :-
    read_line_to_string(user_input,S),
    number_string(N,S),
    X is (2^N+1)^2,
    writeln(X).
main.