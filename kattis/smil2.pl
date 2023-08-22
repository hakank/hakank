% https://open.kattis.com/problems/smil
% CPU Time limit: 1s
% Difficulty: 1.3-1.5 Easy
%
% Trying to make this shorter, i.e. without kattio.pl
% Quite shorter....
main :-
    read_line_to_string(user_input,L),
    atom_string(S,L),
    member(Ss,[':)',';)',':-)',';-)']),
    sub_string(S, B, _, _, Ss),
    writeln(B),
    fail.
main.
