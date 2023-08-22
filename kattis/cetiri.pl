% https://open.kattis.com/problems/cetiri
% 1s
% 1.8 Easy

main :-
    read_line_to_string(user_input,S),
    split_string(S," ","",Ss),
    maplist(number_string,Ns0,Ss),
    msort(Ns0,[A,B,C]),
    D1 is B-A,D2 is C-B,
    (D1 =:= D2 -> 
        X is C+D1
    ;
        (D1 < D2 ->
            X is B+D1
        ;
            X is A+D2
        )
    ),
    writeln(X).

    