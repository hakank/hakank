% https://open.kattis.com/problems/sunandmoon
% 1s
% 1.7 Easy

main :-
    readln([S1,S2,M1,M2],end_of_file),
    s(0,S1,S2,M1,M2).
main.
s(Y,S1,S2,M1,M2) :-
    ( ( (Y+S1) mod S2 =:= 0,(Y+M1) mod M2 =:= 0) ->
        writeln(Y)
    ;
        Y1 is Y+1,
        s(Y1,S1,S2,M1,M2)
    ).

/*
% Compressed: 156 chars: Top 10, place 8
main:-readln([S1,S2,M1,M2],end_of_file),s(0,S1,S2,M1,M2).
s(Y,S1,S2,M1,M2):-(((Y+S1)mod S2=:=0,(Y+M1)mod M2 =:= 0)->writeln(Y);Y1 is Y+1,s(Y1,S1,S2,M1,M2)).
*/

    