% https://open.kattis.com/problems/securedoors
% 1s
% 1.7 Easy

main :-
    once(read_string(user_input,100000,S)),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(c,Ss,All),
    findall([Name,out],member([Name,_],All),Names0),
    sort(Names0,Names),
    s(All,Names).
main.
s([],_).
s([[P,A]|Ls],Ns) :-
    once(select([P,S],Ns,Ns2)),
    ( A == "entry" ->
        S2 = in, (S == in -> W = " (ANOMALY)"; W = ""),
        format('~w entered~s~n',[P,W])
    ;
        S2 = out, (S == out -> W = " (ANOMALY)" ; W = "" ),
        format('~w exited~s~n',[P,W])
    ),
    append(Ns2,[[P,S2]],Ns3),
    s(Ls,Ns3).
c(S,[N,T]) :- split_string(S," ","",[T,N]).