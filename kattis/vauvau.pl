% https://open.kattis.com/problems/vauvau
% 1s
% 1.9 Easy

main :-
    read_string(user_input,100000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[A,B,C,D,P,M,G],Ss),
    p([P,M,G],A,B,C,D).

p([],_,_,_,_).
p([W|Ws],A,B,C,D) :-
    t(A,B,W,V1), t(C,D,W,V2), V is V1+V2,
    nth0(V,[none,one,both],X),
    writeln(X),
    p(Ws,A,B,C,D).
t(A,B,W,V) :- ((W-1) mod (A+B) < A -> V = 1 ; V = 0).
