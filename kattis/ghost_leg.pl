% https://open.kattis.com/problems/ghostleg
% 1s 2048Mb
% 1.4 Easy

main :-
    read_string(user_input,100000000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[N,_|Ns],Ss),
    findall(P1-P2,(member(P1,Ns),P2 is P1+1),Ps),
    findall(Pos,(between(1,N,P),p(Ps,P,Pos)),PosL),
    numlist(1,N,Ls),
    perm(Ls,PosL,[],Perm),
    maplist(writeln,Perm).

perm([],_,P,P).
perm([I|Is],Pos,L0,[P|L]) :-
    nth1(P,Pos,I),
    perm(Is,Pos,L0,L).
    
p([],C,C).
p([P1-P2|Ps],C0,C) :-
    (C0 =:= P1 ->
        C1 = P2
    ;
        (C0 =:= P2 ->
            C1 = P1
        ;
            C1 = C0
        )
    ),
    p(Ps,C1,C).

    
