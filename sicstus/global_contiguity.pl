/*

  Global constraint global contiguity in SICStus Prolog.

  From Global constraint catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_contiguity.html
  """
  Enforce all variables of the VARIABLES collection to be assigned to 
  0 or 1. In addition, all variables assigned to value 1 appear contiguously.
  """

  The implementation of global contiguity below was inspired by 
  Toby Walsh's presentation "Sliding Constraints"
     http://www.cse.unsw.edu.au/~tw/samos/slide.ppt
  where he defines it in terms of the global constraint slide.

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/global_contiguity.mzn
  * Comet   : http://www.hakank.org/comet/global_contiguity.co
  * Gecode  : http://www.hakank.org/gecode/global_contiguity.cpp
  * ECLiPSe : http://www.hakank.org/eclipse/global_contiguity.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-

        N = 20,
        length(X, N),
        domain(X,0,1),

        global_contiguity(X),
        
        labeling([],X),
        write(X), nl,
        fail.



%
% contiguity: all variables assigned to value 1 appear contiguously.
%
global_contiguity(X) :-

        length(X, Len),
        length(Y, Len),
        domain(Y, 0,2),
         
        my_ordered(Y),
        (foreach(XVal,X),
         foreach(YVal,Y) do
             BX in 0..1,
             BY in 0..1,
             (XVal #= 1) #<=> BX #= 1,
             (YVal #= 1) #<=> BY #= 1,
             BX #= BY
        ).


my_ordered(List) :-
        ( List = [] -> 
              true
        ;
              ( fromto(List, [This,Next | Rest], [Next|Rest],[_])
              do
                This #=< Next
              )
        ).
