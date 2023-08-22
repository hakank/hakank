% https://open.kattis.com/problems/faktor
% 1s
% 1.4 Easy
% Shorter. Not much...
main :-
    read_line_to_codes(user_input,S),
    append(X,[32|Y],S),
    maplist(number_codes,[A,B],[X,Y]),
    between(1,10000000000,I),
    X is ceiling(I/A),
    X >= B,
    writeln(I).
main.
