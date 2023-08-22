% https://open.kattis.com/problems/isithalloween
% 1s
% 1.5 Easy

main :-
    readln(S),
    ( (S == ['OCT',31] ; S == ['DEC',25]) ->
        T=yup
    ;
        T=nope
    ),
    writeln(T).

/*
% Compressed: 73 chars
main:-readln(S),((S==['OCT',31];S==['DEC',25])->T=yup;T=nope),writeln(T).
*/

/*
main :-
    read_line_to_string(user_input,S),
    ( (S == "OCT 31" ; S == "DEC 25") -> writeln("yup") ; writeln("nope") ).
*/