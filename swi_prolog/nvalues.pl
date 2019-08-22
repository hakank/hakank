/*

  (Decomposition of) global constraint nvalues in SWI Prolog

  Reference: 
  Clobal Constraint Catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Cnvalues.html
  """
  Purpose
 
      Let N be the number of distinct values assigned to the variables of the 
      VARIABLES collection. Enforce condition N <RELOP> LIMIT to hold.
 
  Example
      (<4,5,5,4,1,5>,=,3)
 
      The nvalues constraint holds since the number of distinct values occurring within 
      the collection 4,5,5,4,1,5 is equal (i.e., RELOP is set to =) to its 
      third argument LIMIT=3.
  """

  Note:
  nvalues/3 is defined in http://hakank.org/swi_prolog/hakank_utils.pl
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   Len = 5,
   length(X,Len),
   X ins 1..Len,
   N in 1..Len,      

   %% It's better to fix N,
   %% otherwise the same X may yield many
   %% solutions when op is not #=.
   N #= 3,

   %% Ensure that there are atmost N distinct values in X
   nvalues(X,#=<,N),
   
   nvalue(N2,X), % Check: how many different values was it?

   flatten([X,N],Vars),
   labeling([],Vars),

   writeln([n=N, n2=N2, x=X]),
   fail,
   nl.

go.

