/*

  Euler problem 46 in SWI Prolog

  """  
  It was proposed by Christian Goldbach that every odd composite number can be 
  written as the sum of a prime and twice a square.

  9 = 7 + 2×1^2
  15 = 7 + 2×2^2
  21 = 3 + 2×3^2
  25 = 7 + 2×3^2
  27 = 19 + 2×2^2
  33 = 31 + 2×1^2

  It turns out that the conjecture was false.

  What is the smallest odd composite that cannot be written as the 
  sum of a prime and twice a square?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler46a
            ],
        run_problems(L).


%%
%% 0.5s
%%
euler46a :-
        findall(I,
                (between(3,2,10000,I),
                 \+ is_prime(I),
                 S is round(sqrt(I/2)),
                 findall(J,
                         (between(1,S,J),
                          Ts is J*J*2,
                          T is abs(I-Ts),
                          is_prime(T)
                         ),
                         Js),
                 Js = []
                ),
                L),
        min_list(L,Min),
        writeln(Min).
               
                
