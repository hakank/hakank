% https://open.kattis.com/problems/lineup
% 1s
% 1.8 Easy

main :-
    readln([_|S],end_of_file),
    sort(0,@=<,S,I),
    reverse(I,D),
    (S == I ->
        T="INCREASING"
    ;
        (S == D ->
            T = "DECREASING"
        ;
            T = "NEITHER"
        )
    ),
    writeln(T).

/*
% Compressed: 130 chars Top 10 place 4
main:-readln([_|S],end_of_file),sort(0,@=<,S,I),reverse(I,D),(S==I->T="INCREASING";(S==D->T="DECREASING";T="NEITHER")),writeln(T).
*/

/*
main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    sort(0,@=<,Ss,Inc),
    sort(0,@>=,Ss,Dec),    
    (Ss == Inc ->
        T="INCREASING"
    ;
        (Ss == Dec ->
            T = "DECREASING"
        ;
            T = "NEITHER"
        )
    ),
    writeln(T).

*/