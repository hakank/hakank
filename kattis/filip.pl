% https://open.kattis.com/problems/filip
% 1s
% 1.4 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ", "",Ss),
    maplist(atom_chars,Ss,Cs),
    maplist(reverse,Cs,Cs2),
    maplist(number_string,[X,Y],Cs2),
    (X >= Y ->
        writeln(X)
    ;
        writeln(Y)
    ).
main.
