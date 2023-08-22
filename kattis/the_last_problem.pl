% https://open.kattis.com/problems/thelastproblem
% 1s
% 1.7 Easy

% Note: readln/2 give errors!
main :-
    read_line_to_string(user_input,S),
    format('Thank you, ~s, and farewell!~n',[S]).


/*
% Compressed: 85 chars (Top 10 is 42..45 chars)
main:-read_line_to_string(user_input,S),format('Thank you, ~s, and farewell!~n',[S]).

*/