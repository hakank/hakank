/*

  Euler Problem 3 in SICStus Prolog

  http://projecteuler.net/index.php?section=problems&id=3
  """
  The prime factors of 13195 are 5, 7, 13 and 29.
  
  What is the largest prime factor of the number 600851475143 ?
  """
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sictus_prolog/
  
*/


:- ensure_loaded(hakank_utils).


go :-
        L = [
             euler3a % ,
             % euler3b
            ],
        run_problems(L).

  
%% 0.001s
euler3a :-
        prime_factors(600851475143, Divisors),
        max_member(Max,Divisors),
        writeln(Max).

%% 0.013s
euler3b :-
        prime_decomp(600851475143,Factors), 
        max_member(Max,Factors),
        writeln(Max).


