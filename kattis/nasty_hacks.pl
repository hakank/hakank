% https://open.kattis.com/problems/nastyhacks
% 1s
% 1.4 Easy

main :-
    read_string(_),
    repeat,
    read_string(S),
    ( S == end_of_file ;
        split_string(S, " ", "", Ss),
        maplist(number_string,[NotA,AProfit,ACost],Ss),
        Ad is AProfit - ACost,
        ( NotA =:= Ad -> writeln("does not matter")
        ;
            (NotA > Ad -> writeln("do not advertise")
            ;
                writeln("advertise")
            )
        ),
        fail
    ).
main.

read_string(S) :-
    read_line_to_string(user_input,S).
