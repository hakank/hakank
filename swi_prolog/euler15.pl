/*

  Euler Problem 15 in SWI Prolog

  Problem 15
  """
  Starting in the top left corner of a 2×2 grid, there are 6 routes 
  (without backtracking) to the bottom right corner.
  
  How many routes are there through a 20×20 grid?
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).
:- use_module(euler_utils).


go :-
        L = [
             euler15a
            ],
        run_problems(L).

%%
%% 0.000s
%%
euler15a :-
        %% prod(21..40) // prod(2..20)
        numlist(21,40,A),
        numlist(2,20,B),
        prodlist(A,ProdA),
        prodlist(B,ProdB),
        Tot is ProdA // ProdB,
        writeln(Tot).
