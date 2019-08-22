/*

  (Decomposition of) global constraint nvalue in SWI Prolog

  From MiniZinc:
  """
  Requires that the number of distinct values in 'x' is 'n'.
  """

  Note:
  
  nvalue/2 is defined in http://hakank.org/swi_prolog/hakank_utils.pl
  
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   Len = 5,
   length(X, Len),
   X ins 1..Len,
   N in 1..Len,      

   nvalue(N,X),

   %% Some extra constraints
   % N #= 4,
   % increasing(X),

   flatten([X,N],Vars),
   labeling([],Vars),

   writeln([n=N, x=X]),
   fail,
   nl.

go.

