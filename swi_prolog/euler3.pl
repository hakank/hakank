/*

  Euler Problem 3 in SWI Prolog

  http://projecteuler.net/index.php?section=problems&id=3
  """
  The prime factors of 13195 are 5, 7, 13 and 29.
  
  What is the largest prime factor of the number 600851475143 ?
  """
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/
  
*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :-
        L = [
             %% euler3a,
             euler3b % ,
             %% euler3c
            ],
        run_problems(L).

  
%% 0.001s
euler3a :-
        abolish_all_tables,
        prime_factors(600851475143, Divisors),
        max_list(Divisors,Max),
        writeln(Max).

%% 0.000s
euler3b :-
        abolish_all_tables,
        prime_decomp(600851475143,Factors), 
        max_list(Factors,Max),
        writeln(Max).

%% 0.001s
euler3c :-
        abolish_all_tables,
        prime_factors(600851475143,Factors), 
        max_list(Factors,Max),
        writeln(Max).
