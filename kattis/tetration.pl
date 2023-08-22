% https://open.kattis.com/problems/tetration
% 1s
% 1.6 Easy

% 70 chars
main :-
    readln([N],end_of_file),
    format("~f",[N^(1/N)]).

/*
% Compressed: 53 chars
main:-readln([N],end_of_file),format("~f",[N^(1/N)]).
*/


/*
main :-
    readln([N],end_of_file),
    T is N^(1/N),
    writeln(T).

% Compressed : 54 chars
main:-readln([N],end_of_file),T is N^(1/N),writeln(T).
*/

/*
% 104 chars
main :-
    read_line_to_string(user_input,S),
    number_string(N,S),
    T is N^(1/N),
    writeln(T).
*/

/*
% Compressed: 83 chars (Top 10 is 20..31)
main:-read_line_to_string(user_input,S),number_string(N,S),T is N^(1/N),writeln(T).

*/
