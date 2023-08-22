% https://open.kattis.com/problems/closingtheloop
% 1s
% 1.7 Easy

% This could probably be written more elegant

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(1,Ss).
main.

s(_,[]).
s(I,[_,S|Ss]) :-
    split_string(S," ","",R),
    maplist(p,R,NC),
    length(NC,Len),
    (Len =:= 1 ->
        format('Case #~d: ~d~n',[I,0])
    ;
        sort(0,@>=,NC,NCS),
        c(NCS,[],Bs,[],Rs),
        t(Bs,Rs,[],Ts),
        sol(Ts,0,Res),
        format('Case #~d: ~d~n',[I,Res])
    ),
    I1 is I+1,
    s(I1,Ss).

sol([],S,S).
sol([_,N|T],S0,S) :-
    S1 is S0+N-1,
    sol(T,S1,S).

t([],[],Res,Res).
t([],_,Res,Res).
t(_,[],Res,Res).
t([B|Bs],[R|Rs],S0,S) :-
    append(S0,["B",B,"R",R],S1),
    t(Bs,Rs,S1,S).

p(S,[N,C]) :- member(C,["R","B"]),string_concat(N0,C,S),number_string(N,N0).

c([],Bs,Bs,Rs,Rs).
c([[N,C]|T],B0,B,R0,R) :-
    (C == "B" ->
        append(B0,[N],B1),
        R1 = R0
    ;
        append(R0,[N],R1),
        B1 = B0
    ),
    c(T,B1,B,R1,R).
