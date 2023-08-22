% https://open.kattis.com/problems/autori
% CPU Time Limit: 1s
% Difficulty: 1.3 Easy
% :- [kattio].

main :-
    read_line_to_string(user_input,Inp),
    atom_string(S,Inp),
    split_string(S, "-", " ", L),
    take_first(L),
    nl.
main.

take_first([],[]).
take_first([S|Ss]) :-
    string_chars(S,Cs),
    [H|_] = Cs,
    write(H),
    take_first(Ss).
