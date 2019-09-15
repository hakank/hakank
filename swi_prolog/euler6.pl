/*

  Euler Problem 6 in SWI Prolog

  http://projecteuler.net/index.php?section=problems&id=6
  """
  The sum of the squares of the first ten natural numbers is,
  12 + 22 + ... + 102 = 385

  The square of the sum of the first ten natural numbers is,
  (1 + 2 + ... + 10)2 = 552 = 3025

  Hence the difference between the sum of the squares of the first ten
  natural numbers and the square of the sum is 3025 385 = 2640.

  Find the difference between the sum of the squares of the first one
  hundred natural numbers and the square of the sum.
  """
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :-
        L = [
             euler6a
            ],
        run_problems(L).

% 0.000s
euler6a :-
        numlist(1,100,List),
        maplist(sum_sq,List,ListS),
        sum(ListS,#=,SumSquares),
        
        sum_list(List, Sum), 
        SquaresSum #= Sum^2, 
        Diff #= SquaresSum - SumSquares,
        writeln(Diff).

sum_sq(S,Sq) :- Sq #= S*S.