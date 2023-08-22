% https://open.kattis.com/problems/tornbygge
% 1s
% 1.8 Easy

main :-
    read_string(user_input,10000000,S),
    split_string(S,"\n ","\n ",[_|Ss]),
    maplist(number_string,Ns,Ss),
    findall(X,(nextto(A,B,Ns),(A<B->X=1;X=0)),Xs),
    sum_list(Xs,V),
    Res is V+1,
    writeln(Res).
