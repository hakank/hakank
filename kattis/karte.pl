% https://open.kattis.com/problems/karte
% 1s
% 1.6 Easy

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(L),user_input),
    (\+d(L) ->
        writeln("GRESKA")
    ;
        s(L,0,P,0,K,0,H,0,T),
        format('~d ~d ~d ~d~n',[13-P,13-K,13-H,13-T])
    ).
main.

s([],P,P,K,K,H,H,T,T).
s([[0'P,_]|Cs],P0,P,K0,K,H0,H,T0,T) :-
    P1 is P0+1,
    s(Cs,P1,P,K0,K,H0,H,T0,T).
s([[0'K,_]|Cs],P0,P,K0,K,H0,H,T0,T) :-
    K1 is K0+1,
    s(Cs,P0,P,K1,K,H0,H,T0,T).
s([[0'H,_]|Cs],P0,P,K0,K,H0,H,T0,T) :-
    H1 is H0+1,
    s(Cs,P0,P,K0,K,H1,H,T0,T).
s([[0'T,_]|Cs],P0,P,K0,K,H0,H,T0,T) :-
    T1 is T0+1,
    s(Cs,P0,P,K0,K,H0,H,T1,T).

d([]).
d([H|T]) :-
    (member(H,T) ->
        fail
    ;
        d(T)
    ).

q1([P,A]) --> [P], {memberchk(P,[0'P,0'K,0'H,0'T])}, integer(A).

q([P|Ps]) --> q1(P),q(Ps).
q([]) --> [].
p(L) --> q(L),eol.