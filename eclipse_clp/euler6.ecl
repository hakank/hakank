/*
  Problem 6
http://projecteuler.net/index.php?section=problems&id=6
The sum of the squares of the first ten natural numbers is,
12 + 22 + ... + 102 = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)2 = 552 = 3025

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

Solution: 25164150

*/

:- lib(lists).
:- lib(listut).
:- lib(util).
:- lib(ic).
:- lib(ic_global).
:- lib(propia).
:- lib(ic_search).
:- lib(hash).


% ok (0.0s)
problem6 :-
        (
            for(I,1,100), 
            foreach(J,List), 
            foreach(K,List2) 
        do 
            J is I^2, 
            K is I
        ), 
        % ic_global:sumlist(List, SumSquares), 
        % ic_global:sumlist(List2, Sum), 
        listut:sumlist(List, SumSquares), 
        listut:sumlist(List2, Sum), 
        SquaresSum is Sum^2, 
        Diff is SquaresSum - SumSquares,
        writeln(Diff).


go :-
        writeln('problem6'),
        problem6.
