% https://open.kattis.com/problems/canadianseh
% 1s
% 1.3 Easy

main :-
    readln(S),
    (append(_,['eh','?'],S)  ->
        T="Canadian!"
    ;
        T="Imposter!"
    ),
    writeln(T).

/*
% Compressed: 81 chars
main:-readln(S),(append(_,['eh','?'],S)->T="Canadian!";T="Imposter!"),writeln(T).

*/

/*
main :-
    read_line_to_codes(user_input,S),
    ( append(_,[0'e,0'h,0'?],S) ->
        writeln("Canadian!")
    ;
        writeln("Imposter!")
    ).
main.
*/

