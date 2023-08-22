% https://open.kattis.com/problems/costumecontest
% 1s
% 1.9 Easy

main :-
    read_string(user_input,1000000,S),
    split_string(S,"\n","\n",[_|Ss]),
    msort(Ss,T),
    clumped(T,Cl),
    sort(2,@=<,Cl,R),
    [_-Min|_] = R,
    forall(member(X-Min,R),writeln(X)).

/*
% Compressed: 165
% which placed in the Top 10 shortest programs! Now in Place 3
main:-read_string(user_input,1000000,S),split_string(S,"\n","\n",[_|Ss]),msort(Ss,T),clumped(T,Cl),
sort(2,@=<,Cl,R),[_-Min|_]=R,forall(member(X-Min,R),writeln(X)).

*/