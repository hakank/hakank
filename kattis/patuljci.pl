% https://open.kattis.com/problems/patuljci
% 1s
% 1.8 Easy

:- use_module(library(clpfd)).
main :-
    read_string(user_input,100000,S),
    split_string(S,"\n","\n",Ss),
    maplist(number_string,Ns,Ss),
    length(X,9),
    X ins 0..1,
    sum(X,#=,7),
    scalar_product(Ns,X,#=,100),
    label(X),
    s(X,Ns).

s([],[]).
s([X|Xs],[N|Ns]) :-
    (X =:= 1 ->
        writeln(N)
    ;
        true
    ),
    s(Xs,Ns).
