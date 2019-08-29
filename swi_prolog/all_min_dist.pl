/*

  Global constraint all_min_dist in SWI Prolog

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Call_min_dist.html
  """
  Enforce for each pair (vari, varj) i ‹ j of distinct variables of the 
  collection VARIABLES that 
  |vari - varj| >= MINDIST.
  
  Example
   (2, <5, 1, 9, 3>)
  
  The all_min_dist constraint holds since the following expressions 
  |5-1|, |5-9|, |5-3|, |1-9|, |1-3|, |9-3| are all greater than or equal 
  to the first argument MINDIST = 2 of the all_min_dist constraint.
  """

  Note:
  all_min_dist/2 is defined in http://hakank.org/swi_prolog/hakank_utils.pl
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        N = 4,
        length(X,N),
        X ins 1..9,
        
        % C in 0..9,
        C #= 2,
        
        findall([x=X,c=C], (
                    all_min_dist(C, X),
                    flatten([X,C], Vars),
                    labeling([],Vars)
                   ),
                L),
        length(L,Len),
        maplist(writeln,L),
        writeln(len=Len),
        nl.
