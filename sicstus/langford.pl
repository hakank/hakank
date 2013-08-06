/*

  Langford's number problem in SICStus Prolog.

  Langford's number problem (CSP lib problem 24)
  http://www.csplib.org/prob/prob024/
  """
  Arrange 2 sets of positive integers 1..k to a sequence,
  such that, following the first occurence of an integer i, 
  each subsequent occurrence of i, appears i+1 indices later
  than the last. 
  For example, for k=4, a solution would be 41312432
  """
  
  * John E. Miller: Langford's Problem
    http://www.lclark.edu/~miller/langford.html
  
  * Encyclopedia of Integer Sequences for the number of solutions for each k
    http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552
 

  Also, see the following models:
  * MiniZinc: http://www.hakank.org/minizinc/langford2.mzn
  * Comet   : http://www.hakank.org/comet/langford.co
  * Gecode/R: http://www.hakank.org/gecode_r/langford.rb
  * ECLiPSe : http://www.hakank.org/eclipse/langford.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        K in 2..8,
        indomain(K),
        write(K),nl,
        findall([K,Solution,Position], langford(K,Solution,Position),
                L),
        length(L,Len),
        write(L),nl,
        write(len:Len),nl,
        nl,
        fd_statistics,
        fail.

langford(K, Solution, Position) :-
        K2 is 2*K,
        length(Position, K2),
        domain(Position,1,K2),
               
        length(Solution,K2),
        domain(Solution,1,K),

        all_different(Position),

        %  symmetry breaking
        nth1(1,Solution,Solution1),
        nth1(K2,Solution,SolutionK2),
        Solution1 #< SolutionK2,

        ( for(I,1,K), 
          param(Position,Solution,K) 
        do
          IK is I+K,
          element(IK, Position, PositionIK),
          element(I, Position, PositionI),
          I1 is I+1,
          PositionIK #= PositionI + I1,
          element(PositionI,Solution,SolutionPositionI),
          SolutionPositionI #= I,
          element(PositionIK,Solution,SolutionPositionIK),
          SolutionPositionIK #= I
        ),

        append(Solution,Position, Vars),
        labeling([ffc,bisect,down], Vars).




