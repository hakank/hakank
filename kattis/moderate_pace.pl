% https://open.kattis.com/problems/moderatepace
% 1s
% 1.6 Easy

% See a shorter Picat program in moderate_pace.pi

:- use_module(library(clpfd)).
main :-
    read_string(user_input,1000000000,S),
    split_string(S,"\n ","\n ",[NS|Ss]),
    number_string(N,NS),
    maplist(number_string,Ns,Ss),
    part(Ns,N,Ms),
    transpose(Ms,Ts),
    s(Ts),
    nl.

s([]).
s([S|Ss]) :-
    median(S,Median),
    format("~d ",[Median]),
    s(Ss).

median(L,Median) :-
    msort(L,Sorted),
    length(L,Len),
    Mid is Len div 2,
    nth0(Mid,Sorted,Median).

part([], _, []).
part(L, N, [DL|DLTail]) :-
    length(DL, N),
    append(DL, LTail, L),
    part(LTail, N, DLTail).
