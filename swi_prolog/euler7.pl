/*

  Euler problem 7 in SWI Prolog

  http://projecteuler.net/index.php?section=problems&id=7
  """
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that
  the 6th prime is 13.

  What is the 10001st prime number?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).


go :-
        L = [
             euler7a %%,
             %% euler7b
            ],
        run_problems(L).

%% 0.34s
euler7a :-
        nth_prime(10001, P),
        writeln(P).

%% clpfd: 1.5s
euler7b :-
        nth_prime_clp(10001, P),
        writeln(P).

