% https://open.kattis.com/problems/oddgnome
% 1s
% 1.7 Easy

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).

s([]).
s([S|Ss]) :-
    split_string(S," ","",[_|Ns0]),
    maplist(number_string,Ns,Ns0),
    length(Ns,Len),
    [First|_] = Ns,last(Ns,Last),
    once(findall(Pos,(between(1,Len,Pos),nth1(Pos,Ns,V),(V<First;V>Last)),Ps)),
    [Pos|_] = Ps,
    writeln(Pos),
    s(Ss).
