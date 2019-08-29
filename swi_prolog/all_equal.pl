/*

  Global constraint all_equal in SWI Prolog

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Call_equal.html
  """
  Constraint
 
      all_equal(VARIABLES)
 
  Purpose
 
      Enforce all variables of the collection VARIABLES to take the same value.
 
  Example
      (<5, 5, 5, 5>)
 
  The all_equal constraint holds since all its variables are fixed to value 5.
  """

  all_equal/1 is defined in http://hakank.org/swi_prolog/hakank_utils.pl
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   N = 5,
   Lower = 0,
   Upper = 6,
   findall(X, all_equal_test(N,Lower, Upper,X),List),
   length(List,Len),
   writeln(List),
   writeln(length=Len),
   nl.

all_equal_test(N, Lower, Upper, X) :-
   length(X,N),
   X ins Lower..Upper,
   all_equal(X),
   labeling([],X).

