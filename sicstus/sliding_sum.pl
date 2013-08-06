/*

  Global constraint sliding sum in SICStus Prolog.

  From Global Constraint Catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Csliding_sum.html
  """
  sliding_sum(LOW, UP, SEQ, VARIABLES)
  
  Purpose
 
  Constrains all sequences of SEQ consecutive variables of the collection 
  VARIABLES so that the sum of the variables belongs to interval [LOW, UP].
 
  Example
      (
      3, 7, 4,<1, 4, 2, 0, 0, 3, 4>
      )
 
  The example considers all sliding sequences of SEQ=4 consecutive values of 
  <1, 4, 2, 0,0,3, 4> collection and constraints the sum to be in 
  [LOW,UP] = [3, 7]. The sliding_sum constraint holds since the sum associated 
  with the corresponding subsequences 1 4 2 0, 4 2 0 0, 2 0 0 3, and 
  0 0 3 4 are respectively 7, 6, 5 and 7.
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/sliding_sum.mzn
  * Comet   : http://www.hakank.org/comet/sliding_sum.co
  * ECLiPSe : http://www.hakank.org/eclipse/sliding_sum.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 7,
        length(Variables, N),
        domain(Variables,0,4),

        Low in 0..10,
        Up in 0..10,

        Seq in 1..N,
        Seq #= 4,

        % test data:
        % Variables = [1,4,2,0,0,3,3],
        Low #= 3,
        Up #= 7,

        sliding_sum(Low, Up, Seq, Variables),

        append(Variables,[Low,Up,Seq], Vars),
        labeling([],Vars),

        write(variables:Variables),nl,
        % write([low:Low, up:Up, seq:Seq]),nl,
        fail.


%
% Note: Seq must be instantiated, but neither Low or Up has
% to be (the result may be weird unless they are, though).
%
sliding_sum(Low, Up, Seq, Variables) :-
        length(Variables, VarLen),
        ( for(I,1,VarLen-Seq+1), 
          param(Variables, Low, Up, Seq) do
              ( for(J,I,I+Seq-1),
                fromto(0,In,Out,Sum),
                param(Variables) do
                    element(J,Variables, V),
                    Out #= In + V
              ),
              S #= Sum,
              S #>= Low,
              S #=< Up
        ).
