cons(A,B,C):-
    append([A],B,C).
tail([_|T],T).
head([H|_],H).
sum(A,B,C):-
    C is A+B.
empty([]).
zero(0).

% ORIGINAL but it throws error
% even(0):- !.
% even(1) :- !,false.
% even(N):- M is N - 2, even(M).
% odd(0) :- !,false.
% odd(1) :- !.
% odd(N):- M is N - 2, odd(M).

% hakank: fixed the above.
% even(0):- !.
% even(1) :- !,false.
% even(N):- integer(N), M is N - 2, even(M).
% odd(0) :- !,false.
% odd(1) :- !.
% odd(N):- integer(N), M is N - 2, odd(M).


% hakank
even(N) :- integer(N), 0 is N mod 2.
odd(N) :- integer(N), 1 is N mod 2.
