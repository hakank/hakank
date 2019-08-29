/*

  Global constraint among_seq in SWI Prolog

  From Global constraint catalog:
  http://www.emn.fr/x-info/sdemasse/gccat/Camong_seq.html
  """
  Constraint

    among_seq(LOW,UP,SEQ,VARIABLES,VALUES)

  Purpose  
  Constrains all sequences of SEQ consecutive variables of the collection 
  VARIABLES to take at least LOW values in VALUES and at most UP values 
  in VALUES.

  Example
    (
    1,2,4,<9,2,4,5,5,7,2>,
    <0,2,4,6,8>
    )

  The among_seq constraint holds since the different sequences of 4 
  consecutive variables contains respectively 2, 2, 1 and 1 even numbers.
  """

  Note:
  among_seq/5 is defined in http://hakank.org/swi_prolog/hakank_utils.pl
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   Len = 7,

   V = [0,2,4,6,8],

   length(X,Len),
   X ins 0..9,

   %% the example above
   %% (don't work with all_different/1 and increasing/1 though)
   % X = [9,2,4,5,5,7,2], 

   % some symmetry breaking if we let X free
   all_different(X),
   increasing(X),

   Low = 1,
   High = 2,
   SeqLen = 4,
   among_seq(Low,High,SeqLen,X,V),

   label(X),

   writeln([low=Low,high=High,seq_len=SeqLen]),
   writeln(x=X),
   writeln(v=V),
   nl,
   fail.

go.

