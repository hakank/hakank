% https://open.kattis.com/problems/symmetricorder
% 1s
% 1.6 Easy

% 1,2,3,4,5 ->  1,3, 5, 4,2
% i.e.    -> odd in increasing order  even in reversed order
%   findall(I,(between(1,N,I),I mod 2 =:= 1),Odd),
%   findall(I,(between(1,N,I),I mod 2 =:= 0),Even0),
% A littler shorter using partition/4


:- use_module(library(dcg/basics)).
main :-
    once(phrase_from_stream(p(L),user_input)),
    s(1,L),
    nl.
main.

f(L,I) :-
    nth1(I,L,V),
    format('~s~n',[V]).

s(_,[_]).
s(_,[0,[]]).
s(C,[[N,L]|Ls]) :-
    N > 0,
    format('SET ~d~n',[C]),
    numlist(1,N,T),
    partition(mod2,T,Odd,Even0),
    reverse(Even0,Even),
    append([Odd,Even],L2),
    maplist(f(L),L2),
    C2 is C+1,
    s(C2,Ls).

mod2(X) :- 1 is X mod 2.

r([S|Ss]) --> string_without("\n",S),{S \= []},eol,r(Ss).
r([S]) --> string_without("\n",S).
r([]) --> [].

q([N,L]) --> integer(N), eol, r(L), {length(L,N)}.
q([]) --> [].

p([L|Ls]) --> q(L), {L \= []}, eol,  p(Ls).
p([L]) --> q(L).
p([]) --> [].
