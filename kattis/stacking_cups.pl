% https://open.kattis.com/problems/cups
% 1s
% 1.6 Easy

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(L),user_input),
    sort(L,S),
    f(S).
main.

f([]).
f([[_,T]|Ls]) :-
    format('~s~n',[T]),
    f(Ls).

q1([V,T]) --> integer(V)," ", string(T).
q1([V,T]) --> string(T), " ", integer(V0), {V is V0*2}.

q([L|Ls]) --> q1(L), {L \= []}, eol, q(Ls).
q([L]) --> q1(L).
q([]) --> [].

p(L) --> integer(_), eol,q(L), eol.
