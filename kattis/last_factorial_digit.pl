% https://open.kattis.com/problems/lastfactorialdigit
% 1s
% 1.5 Easy

:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(L),user_input),
    maplist(f,L).
main.

f(1) :- writeln(1),!.
f(N) :-
    numlist(2,N,Is),
    foldl(mult,Is,1,F),
    number_chars(F,Cs),
    last(Cs,C),
    writeln(C).

mult(A,B,C) :- C is A*B.

q([I|Is]) --> integer(I), eol, q(Is).
q([I]) --> integer(I).
q([]) --> [].
p(L) --> integer(_), eol, q(L).


