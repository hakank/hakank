/*

  Global constraint sliding sum in B-Prolog.

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


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :-
        N = 7,
        length(Variables, N),
        Variables :: 0..4,

        Low :: 0..10,
        Up :: 0..10,

        Seq :: 1..N,
        Seq #= 4,

        % test data:
        % Variables = [1,4,2,0,0,3,3],
        Low #= 3,
        Up #= 7,

        sliding_sum(Low, Up, Seq, Variables),

        term_variables([Variables,Low,Up,Seq], Vars),
        labeling([],Vars),

        writeln(variables:Variables),
        fail.


%
% Note: Seq must be instantiated, but neither Low or Up has
% to be (the result may be weird unless they are, though).
%
sliding_sum(Low, Up, Seq, Variables) :-
        length(Variables, VarLen),
        foreach(I in 1..VarLen-Seq+1, 
                [Sum],
                (
                    Sum #= sum([V : J in I..I+Seq-1,[V],
                                element(J,Variables, V)]),
                    Sum #>= Low,
                    Sum #=< Up
                )
        ).


