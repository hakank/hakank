% https://open.kattis.com/problems/fallingapart
% 1s
% 1.7 Easy

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    sort(0,@>=,Ns,NsS),
    s(1,NsS,0,A,0,B),
    format('~d ~d~n',[A,B]).

s(_,[],A,A,B,B).
s(Who,[N|Ns],A0,A,B0,B) :-
    (Who =:= 1 ->
        A1 is A0 + N,
        B1 is B0
    ;
        B1 is B0 + N,
        A1 is A0
    ),
    opp(Who,Who2),
    s(Who2,Ns,A1,A,B1,B).

opp(1,2).
opp(2,1).
        

/*
Compressed: 319 chars

main:-read_string(user_input,10000000,S),split_string(S,"\n ","\n ",[_|Ss]),maplist(number_string,Ns,Ss),sort(0,@>=,Ns,NsS),s(1,NsS,0,A,0,B),format('~d ~d~n',[A,B]).
s(_,[],A,A,B,B). s(Who,[N|Ns],A0,A,B0,B):-(Who=:=1->A1 is A0 + N,B1 is B0;B1 is B0 + N,A1 is A0),opp(Who,Who2),s(Who2,Ns,A1,A,B1,B).
opp(1,2). opp(2,1).

        
*/