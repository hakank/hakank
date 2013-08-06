/*

  Global constraint global contiguity in B-Prolog.

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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        N = 20,
        length(X, N),
        X :: 0..1,

        global_contiguity(X),
        
        labeling([],X),
        writeln(X), 
        nl,
        fail.



%
% contiguity: all variables assigned to value 1 appear contiguously.
%
global_contiguity(X) :-

        length(X, Len),
        length(Y, Len),
        Y :: 0..2,
         
        increasing(Y),
        foreach((XVal,YVal) in (X,Y),
                [BX,BY],
                (
                    BX in 0..1,
                    BY in 0..1,
                    (XVal #= 1) #<=> BX #= 1,
                    (YVal #= 1) #<=> BY #= 1,
                    BX #= BY
                )
        ).


increasing(List) :-
        foreach(I in 2..List^length, List[I-1] #=< List[I]).
