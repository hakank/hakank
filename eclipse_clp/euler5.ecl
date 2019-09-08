/*

  Problem 5
http://projecteuler.net/index.php?section=problems&id=5
2520 is the smallest number that can be divided by each of the numbers
from 1 to 10 without any remainder.

What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?

Answer: 232792560
*/

:- lib(lists).
:- lib(listut).
:- lib(util).
:- lib(ic).
:- lib(ic_global).
:- lib(propia).
:- lib(ic_search).
:- lib(hash).


% N is divisible by all numbers 2..D
divisible_by_range(N, D) :-
        for(I, 2, D), param(N) do M is N mod I, M = 0.


foldr(_Op, Init, [], Init).
foldr(Op, Init, [X|Xs], R) :- 
        foldr(Op, Init, Xs, R1), 
        P =.. [Op, X, R1, R], 
        call(P).

% ok (0.0s)
problem5 :-
        numlist(1,20, List),
        foldr(lcm, 1, List, As),
        writeln(As).

% alternative with do-loops (0.0s)
problem5b :-
        ( for(I,1,20),
          fromto(1,In,Out,Lcm) do
              Out is lcm(I,In)
          ),
        writeln(Lcm).



go :-
        writeln('problem5'),
        problem5,
        writeln('problem5b'),
        problem5b.