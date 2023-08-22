% https://open.kattis.com/problems/mathhomework
% 1s
% 1.9 Easy

:- use_module(library(clpfd)).
main :-
    read_line_to_string(user_input,S),
    split_string(S," "," ",Ss),
    maplist(number_string,[A,B,C,T],Ss),
    length(X,3),X ins 0..200000,
    findall(X,(scalar_product([A,B,C],X,#=,T),labeling([ff,bisect],X)),Xs),
    (Xs \= []->  msort(Xs,Sort), maplist(format("~d ~d ~d~n"),Sort); writeln("impossible")).
