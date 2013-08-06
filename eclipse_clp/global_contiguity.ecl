/*

  Global constraint global contiguity in ECLiPSe.

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


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).


go :-

        N = 10,
        length(X, N),
        X :: 0..1,

        global_contiguity(X),
        
        labeling(X),
        writeln(X), 

        fail.



%
% contiguity: all variables assigned to value 1 appear contiguously.
%
global_contiguity(X) :-

        length(X, Len),
        length(Y, Len),
        Y :: 0..2,
         
        ordered(=<, Y),
        (foreach(XVal,X),
         foreach(YVal,Y) do
             (XVal #= 1) #= (YVal #= 1)
        ).
