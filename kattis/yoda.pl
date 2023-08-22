% https://open.kattis.com/problems/yoda
% 1s
% 2,1 Easy

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[AS,BS]),
    maplist(string_codes,[AS,BS],[A0,B0]),
    length(A0,L1),
    length(B0,L2),
    D is abs(L1-L2),
    (L1 < L2 -> f(A0,D,A), B = B0 ; (L1 > L2 -> f(B0,D,B), A = A0 ; A = A0, B = B0 ) ),
    s(A,B,[],AR,[],BR), w(AR),w(BR).
w(R) :-
    (R == [] -> writeln("YODA") ; number_string(D,R), format("~d~n",[D])).
s([],[],A,A,B,B).
s([X|Xs],[Y|Ys],A0,A,B0,B) :-
    (X < Y -> A1 = A0, append(B0,[Y],B1) ;
        (X > Y -> append(A0,[X],A1), B1 = B0 ;
            append(A0,[X],A1), append(B0,[Y],B1)  )
    ),
    s(Xs,Ys,A1,A,B1,B).
f(L,N,L2) :-
    findall(0'0,between(1,N,_),F),
    append(F,L,L2).
     