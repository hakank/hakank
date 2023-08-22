% https://open.kattis.com/problems/faktor
% 1s
% 1.4 Easy
main :-
    read_line_to_string(user_input,S),
    split_string(S," ", "", Ss),
    maplist(number_string,[A,B],Ss),
    between(1,10000000000,I),
    X is ceiling(I/A),
    X >= B,
    writeln(I).
main.
