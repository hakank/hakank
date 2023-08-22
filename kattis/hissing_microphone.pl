% https://open.kattis.com/problems/hissingmicrophone
% 1s
% 1.4 Easy

main :-
    read_line_to_codes(user_input,S),
    (append(_,[0's,0's|_],S) ->
        T=hiss
    ;
        T="no hiss"
    ),
    writeln(T).

/*
% Compressed: 96 chars
main:-read_line_to_codes(user_input,S),(append(_,[0's,0's|_],S)->T=hiss;T="no hiss"),writeln(T).
*/