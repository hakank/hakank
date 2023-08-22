% https://open.kattis.com/problems/sok
% 1s
% 1.8 Easy

:- use_module(library(clpr)).
main :-
    read_string(user_input,100000,S),
    split_string(S,"\n ","\n ",Ss),
    maplist(number_string,[A,B,C,I,J,K],Ss),
    s([A,B,C,I,J,K]).

s([A,B,C, I,J,K]) :-
    Tot is I+J+K,
    {Orange > 0, Apple > 0, Pineapple > 0,
     Orange =< A, Apple =< B, Pineapple =< C,
     T = Orange + Apple + Pineapple,
     Orange*Tot = I*T,
     Apple*Tot = J*T,
     Pineapple*Tot = K*T
    },
    maximize(T),
    AD is (A-Orange),
    BD is (B-Apple),
    CD is (C-Pineapple),
    maplist(format('~6f '),[AD,BD,CD]),
    nl.
