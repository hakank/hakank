/*

  Euler problem 7 in SICStus Prolog

  http://projecteuler.net/index.php?section=problems&id=7
  """
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that
  the 6th prime is 13.

  What is the 10001st prime number?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus_prolog/

*/

:- ensure_loaded(hakank_utils).


go :-
        L = [
             euler7a % ,
             % euler7b,
             % euler7c
            ],
        run_problems(L).

%% 0.043s
euler7a :-
        nth_prime(10001, P),
        writeln(P).

%% clpfd: 0.710s
euler7b :-
        nth_prime_clp(10001, P),
        writeln(P).


% With a little cheating: 0.616s
euler7c :-
    primes(200000,L),
    nth1(10001,L,P),
    writeln(P).


