% https://open.kattis.com/problems/password
% 1s
% 2.1 Easy

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[_|Ss]),
    maplist(t,Ss,Ps0),
    sort(0,@>=,Ps0,Ps),
    writeln(ps=Ps),
    s(1,Ps,0,R),
    writeln(R).
t(S,P) :- split_string(S," ","",[_,P0]),number_string(P,P0).
s(_,[],R,R).
s(I,[P|Ps],R0,R) :-
    writeln(s(I,[P|Ps],R0,R)),
    R1 is R0 + I*P,
    I1 is I+1,
    s(I1,Ps,R1,R).

