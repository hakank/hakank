/*

  Euler Problem 5 in SWI Prolog

  http://projecteuler.net/index.php?section=problems&id=5
  """
  2520 is the smallest number that can be divided by each of the 
  numbers from 1 to 10 without any remainder.

  What is the smallest number that is evenly divisible by all of 
  the numbers from 1 to 20?
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :-
        L = [
             euler5a
            ],
        run_problems(L).

% 0.0s
euler5a :-
        numlist(2,20,Ls),
        foldl(lcm, Ls, 1, Res),
        writeln(Res).

