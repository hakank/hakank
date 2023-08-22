% https://open.kattis.com/problems/sodaslurper
% 1s
% 1.8 Easy

main :-
    read_string(user_input,1000000,S),
    split_string(S," ","\n",Ss),
    maplist(number_string,[E,F,C],Ss),
    N is E+F,
    s(N,0,C,0,Res),
    writeln(Res).
s(N,R,C,S,S) :-
    N+R < C.
s(N,R,C,S0,S) :-
    Tot is N+R,
    N1 is Tot div C,
    R1 is Tot-N1*C,
    S1 is S0+N1,
    s(N1,R1,C,S1,S).
