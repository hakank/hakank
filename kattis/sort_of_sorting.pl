% https://open.kattis.com/problems/sortofsorting
% 2s
% 1.8 Easy

main :-
    read_string(user_input,100000000,S),
    split_string(S,"\n","\n",Ss),
    s(Ss).
s([]).
s(["0"]).
s([S|Ss]) :-
    number_string(N,S),
    length(A,N), append(A,Rs,Ss),
    maplist(t,A,Ts),
    sort(1,@=<,Ts,TsS),
    maplist(w,TsS),nl,
    s(Rs).
w([[_,_],L]) :- format("~s~n",[L]).
t(L,[[A,B],Cs]) :- string_codes(L,Cs),[A,B|_] = Cs.