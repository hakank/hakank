% https://open.kattis.com/problems/heirsdilemma
% 1s
% 1.7 Easy

% Trying to make it faster by brute force (and all_different/1)
% Ah, checking d/2 before all_different/1: 0.37s. OK!

:- use_module(library(clpfd)).
main :-
    read_string(user_input,100000,S),
    split_string(S," ","\n",Ss),
    maplist(number_string,[Low,Up],Ss),
    s(Low,Up,0,C),
    writeln(C).

s(I,Up,C,C) :- I > Up.
s(I,Up,C0,C) :-
    number_codes(I,S),
    ( (d(S,I),all_different(S)) ->
        C1 is C0 + 1
    ;
        C1 is C0
    ),
    I1 is I+1,
    s(I1,Up,C1,C).

d([],_).
d([D|Ds],N) :-
    DD is D - 48,
    DD > 0,
    N mod DD =:= 0,
    d(Ds,N).
    