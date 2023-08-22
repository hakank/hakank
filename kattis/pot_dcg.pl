% https://open.kattis.com/problems/pot
% 1s
% 1.3 Easy

% This is one character longer than the "compressed" pot2.pl,
% but much nicer to read.
:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p(Ns),user_input),
    sumlist(Ns,Sum),
    format('~d~n',[Sum]).
main.
q([N|Ns]) --> integer(N1), {N is (N1 div 10)^(N1 mod 10)}, eol, q(Ns).
q([]) --> [].
p(L) --> integer(_), eol, q(L).