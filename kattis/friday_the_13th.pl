% https://open.kattis.com/problems/friday
% 1s
% 2.0 Easy

% All year start on a Sunday (0)
% If "January" have at least 13 days, then it's a Friday.
main :-
    readln([_|S],end_of_file),
    s(S).
s([]).
s([_D,M|Ss]) :-
    length(Ms,M),
    append(Ms,Rs,Ss),
    f(Ms,0,0,F),
    writeln(F),
    s(Rs).
f([],_,F,F).
f([M|Ms],D0,F0,F) :-
    (M < 13 ->
        F1 is F0
    ;
        D13 is (D0 + 13-1) mod 7,
        (D13 =:= 5 ->
            
            F1 is F0+1
        ;
            F1 is F0
        )
    ),
    D1 is (D0+M) mod 7,
    f(Ms,D1,F1,F).


/*
% Compressed: 248 chars. Not short enough for Top 10 (172..210 chars)
main:-readln([_|S],end_of_file),s(S).
s([]). s([_D,M|Ss]):-length(Ms,M),append(Ms,Rs,Ss),f(Ms,0,0,F),writeln(F),s(Rs).
f([],_,F,F). f([M|Ms],D0,F0,F):-
(M<13->F1 is F0;T is(D0+13-1)mod 7,(T=:=5->F1 is F0+1;F1 is F0)),D1 is(D0+M)mod 7,f(Ms,D1,F1,F).
*/
