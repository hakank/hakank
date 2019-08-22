/*

  Global constraint sliding sum in SWI Prolog

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

  Note:
  sliding_sum/[3,4] are defined in http://hakank.org/swi_prolog/hakank_utils.pl
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Testing sliding_sum/4.
%%
go :-

        N = 7,
        length(Variables,N),
        Variables ins 0..4,

        Low in 0..10,
        Up in 0..10,

        Seq in 1..N,
        Seq #= 4,

        Low #= 4,
        Up #= 7,

        sliding_sum(Low, Up, Seq, Variables),

        flatten([Variables,Low,Up,Seq],Vars),
        labeling([],Vars),

        writeln([seq=Seq,low=Low,up=Up,variables=Variables]),
        fail.

go.


%%
%% Testing of sliding_sum/3.
%%
go2 :-

        N = 13,
        length(Variables,N),
        Variables ins 0..4,

        Seq in 1..N,
        Seq #= 3,

        Sum in 0..9,
        Sum #= 4,
        
        sliding_sum(Sum, Seq, Variables),

        flatten([Variables,Sum],Vars),
        labeling([],Vars),

        writeln([seq=Seq,sum=Sum,variables=Variables]),
        fail.

go2.

