/*

  Global constraint alldiffer_on_intersection in SWI Prolog

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_on_intersection.html
  """
  The values that both occur in the VARIABLES1 and VARIABLES2 collections 
  have only one occurrence.
  
  Example
  (
   <5, 9, 1, 5>,
   <2, 1, 6, 9, 6, 2>
  )
  
  The alldifferent_on_intersection constraint holds since the values 9 and 1 
  that both occur in <5, 9, 1, 5> as well as in <2, 1, 6, 9, 6, 2> have 
  exactly one occurrence in each collection.
  """

  Note: alldiffer_on_intersection/2 is defined in http://hakank.org/swi_prolog/hakank_utils.pl
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   M = 4,
   N = 6,

   length(X,M),
   X ins 1..9,

   length(Y,N),
   Y ins 1..9,

   % X = [5,9,1,5],
   X = [5,9,1,_],

   Y = [2,1,6,9,6,2], % constraint holds

   %% Y = [2,1,6,9,6,1], % constraint do not hold since 
   %%                    % there are two 1's in Y and one 1 in X

   alldiffer_on_intersection(X,Y),

   flatten([X,Y],Vars),
   labeling([],Vars),

   writeln(x=X),
   writeln(y=Y),
   nl,
   fail.

go.


go2 :-
   M = 3,
   N = 3,

   length(X,M),
   X ins 1..3,
   X = [1,3,_],

   length(Y,N),
   Y ins 0..2,
   Y = [1,_,2],
   
   alldiffer_on_intersection(X,Y),

   flatten([X,Y],Vars),
   labeling([],Vars),

   writeln(x=X),
   writeln(y=Y),
   nl,
   fail.

go2.


