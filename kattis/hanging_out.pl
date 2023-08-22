% https://open.kattis.com/problems/hangingout
% 1s
% 1.6 Easy

main :-
    read_string(user_input,10000,S),
    split_string(S,"\n","\n",[Ns0|Ss]),
    split_string(Ns0," ", "", Ns),
    maplist(number_string,[Max,_],Ns),
    s(Ss,Max,0,0,Res),
    writeln(Res).
main.

s([],_,_,S,S).
s([L|Ls],Max,T0,S0,S) :-
    (string_concat("enter ",X,L) ->
        number_string(N,X),
        C is T0+N
    ;
        (string_concat("leave ",X,L) ->
            number_string(N,X),
            C is T0-N
        ;
            fail
        )
    ),
    (C =< Max ->
        T1 = C,
        S1 is S0
    ;
        T1 = T0,
        S1 is S0+1
    ),
    s(Ls,Max,T1,S1,S).