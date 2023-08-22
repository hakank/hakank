% https://open.kattis.com/problems/oddecho
% CPU time limit: 1s
% Difficulty: 1.3-1.4 Easy

:- [kattio].

main :-
    read_int(NumLines),
    read_string(X),
    check_line(X,NumLines,1).



check_line(X,NumLines,LineNum) :-
    ( X == end_of_file
    ;
        (LineNum mod 2 =:= 1 ->
            format("~s~n",[X])
        ;
            true
        ),
        read_string(X2),
        NewLineNum is LineNum + 1,
        check_line(X2,NumLines,NewLineNum)
    ).

