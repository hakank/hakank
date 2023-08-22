% https://open.kattis.com/problems/zanzibar
% 1s
% 1.5 Easy

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(L),user_input),
    c(L).
main.

c([]).
c([L1|Ls]) :-
    append(L2,[0],L1),
    findall(T,(nextto(A,B,L2), (B > A*2 -> T is B-A*2 ; T is 0)),Ts),
    sumlist(Ts,Sum),
    writeln(Sum),
    c(Ls).
q1([L|Ls]) --> integer(L)," ",q1(Ls).
q1([L]) --> integer(L).
q1([]).

q([L|Ls]) --> q1(L),eol, q(Ls).
q(L) --> q1(L).
q([]) --> [].

p(L) --> integer(_), eol, q(L).