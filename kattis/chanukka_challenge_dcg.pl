% https://open.kattis.com/problems/chanukah
% 1s
% 1.4 Easy

% Shorter (340 chars) and slighyly slower than chanukah_challenge2.pl: 3.2s
% Still too slow for 1s!
:- use_module(library(dcg/basics)).
main :-
    phrase_from_stream(p,user_input).
main.
q --> integer(L), " ", integer(N), {sum_all(1,N,N,T), format('~w ~w~n',[L,T]) },
      eol,
      q.
q --> [].
p --> integer(_Num), eol, q.

sum_all(N,N,S,S2) :- S2 is S + N.
sum_all(I,N,S0,S) :-
    S1 is S0+I,
    I1 is I+1,
    sum_all(I1,N,S1,S).

