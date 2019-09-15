/*

  Euler problem 47 in SWI Prolog

  """  
  The first two consecutive numbers to have two distinct prime factors are:

  14 = 2 x 7
  15 = 3 x 5

  The first three consecutive numbers to have three distinct 
  prime factors are:

  644 = 2^2 x 7 x 23
  645 = 3 x 5 x 43
  646 = 2 x 17 x 19.

  Find the first four consecutive integers to have four distinct primes 
  factors. What is the first of these numbers?
  """ 

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).

go :- 
        L = [
             euler47a
             %% euler47b
            ],
        run_problems(L).


%%
%% 4.2s
%%
euler47a :-
        abolish_all_tables,
        find_four_consecutive_numbers_n(1,0,Res),
        writeln(Res).


%%
%% 12.9s
%%
euler47b :-
        abolish_all_tables,        
        findall(I,
                (between(1,300000,I),
                 prime_factors_cache(I,F1),
                 sort(F1,F),
                 length(F,4)
                ),
                L),
        find_four_consecutive_numbers(L,[],Res),
        writeln(Res).



find_four_consecutive_numbers([],Res,Res).
find_four_consecutive_numbers([X1,X2,X3,X4|Res],L0,L) :-
        ( (X2 is X1 + 1,
           X3 is X2 + 1,
           X4 is X3 + 1
          )
        ->
          L1 = X1,
          find_four_consecutive_numbers([],L1,L)
        ;
          L1 = L0,
          Res1 = [X2,X3,X4|Res],
          find_four_consecutive_numbers(Res1,L1,L)
        ).

find_four_consecutive_numbers_n(0,S,S).
find_four_consecutive_numbers_n(N,S0,S) :-
        (
         (
          prime_factors_cache(N,F),
          length(F,4),
        
          N1 is N + 1,
          prime_factors_cache(N1,F1),
          length(F1,4),
          
          N2 is N + 2,
          prime_factors_cache(N2,F2),
          length(F2,4),
          
          N3 is N + 3,
          prime_factors_cache(N3,F3),
          length(F3,4)
         )
        ->
          find_four_consecutive_numbers_n(0,N,S)
        ;
         N1 is N+1,
         find_four_consecutive_numbers_n(N1,S0,S)
        ).

:- table prime_factors_cache/2.
prime_factors_cache(N,F) :-
        prime_factors(N,F1),
        sort(F1,F).