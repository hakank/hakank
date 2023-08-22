% https://open.kattis.com/problems/squarepeg
% 1s
% 1.7 Easy

main :-
    readln([L,R]),
    (L =< R*sqrt(2) -> T=fits;T=nope),
    writeln(T).

/*
% Compressed: 61 chars Top 10 place 8
main:-readln([L,R]),(L=<R*sqrt(2)->T=fits;T=nope),writeln(T).
*/

/*
main:-read_line_to_string(user_input,S), split_string(S," ","",Ss),
    maplist(number_string,[L,R],Ss),
    (L =< R*sqrt(2) -> T=fits;T=nope),
    writeln(T).
*/