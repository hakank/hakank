% https://open.kattis.com/problems/simone
% 1s
% 2.0 Easy

main :-
    read_string(user_input,100000000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[_,K|Ns],Ss),
    findall(V,(between(1,K,V),not(memberchk(V,Ns))),NsA),
    (NsA \= [] ->
        length(NsA,Len),
        L = NsA
    ;
        msort(Ns,SsS),
        clumped(SsS,Cl),
        sort(2,@=<,Cl,ClS),
        [_-Min|_] = ClS,
        findall(X,member(X-Min,ClS),L),
        length(L,Len)
    ),
    writeln(Len),
    maplist(format("~d "),L),
    nl.

