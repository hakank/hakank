% https://open.kattis.com/problems/warehouse
% 1s
% 2.1 Easy

main :-
    read_string(user_input,_,S),
    split_string(S,"\n","\n",[_|Ss]),
    s(Ss).
s([]).
s([S|Ss]) :-
    number_string(N,S),
    length(A,N),
    append(A,Rest,Ss),
    maplist(p,A,A2),
    g(A2,[],T),
    sort(1,@=<,T,T2),
    sort(2,@>=,T2,Sorted),
    length(Sorted,Len),    
    writeln(Len),
    maplist(w,Sorted),
    s(Rest).
w(N-C) :- format("~s ~d~n",[N,C]).
g([],L,L).
g([X1-C0|Xs],L0,L) :-
    (select(X1-C1,L0,V) ->
        C2 is C0+C1,
        append([X1-C2],V,L1)        
    ;
        append([X1-C0],L0,L1) 
    ),
    g(Xs,L1,L).
p(L,N-C) :-
    split_string(L," ","",[N,C0]),
    number_string(C,C0).