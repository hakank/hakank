% https://open.kattis.com/problems/metronome
% 1s
% 1.2 Easy

% 62 chars
main :-
    readln([N],end_of_file),
    format("~f",[N/4.0]).

/*
% Compressed: 51 chars Top 10 is 10..13 chars (only APL and Ruby)
main:-readln([N],end_of_file),format("~f",[N/4.0]).
*/

/*
% 104 chars
main :-
    read_line_to_string(user_input,S),
    number_string(N,S),
    X is N / 4.0,
    writeln(X).
*/
