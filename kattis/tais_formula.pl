% https://open.kattis.com/problems/taisformula
% 1s
% 1.6 Easy

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(L),user_input),
    findall(P1-P2,nextto(P1,P2,L),Ps),
    s(Ps,0,S),
    S2 is S / 1000,
    writeln(S2).
main.

s([],S,S).
s([[T1,V1]-[T2,V2]|Ps],S0,S) :-
    S1 is S0 +  ((V1+V2)/2)*(T2-T1),
    s(Ps,S1,S).

q1([A,B]) --> integer(A), " ", float(B).

q([L|Ls]) --> q1(L), eol, q(Ls).
q([L]) --> q1(L).
q([]) --> [].

p(L) --> integer(_), eol, q(L).