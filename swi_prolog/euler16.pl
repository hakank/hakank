/*

  Euler Problem 16 in SWI Prolog

  Problem 16
  """
  2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
  
  What is the sum of the digits of the number 2^1000?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler16a
            ],
        run_problems(L).

%%
%% 0.000s
%%
euler16a :-
        N is 2^1000,
        digits_sum(N,Sum),
        writeln(Sum).

