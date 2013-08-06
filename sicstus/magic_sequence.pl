/*

  Magic sequence in SICStus Prolog.

  From 
  http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
  """
  A magic sequence of length n is a sequence of integers x0 . . xn-1
  between 0 and n-1, such that for all i in 0 to n-1, the number i 
  occurs exactly xi times in the sequence. 
  For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence since 0 occurs 
  6 times in it, 1 occurs twice, ...
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/magic_sequence.mzn
  * Comet   : http://www.hakank.org/comet/magic_sequence.co
 
  Also, in the SICStus Prolog distribution there is a model
  using global_cardinality: 
     library/clpfd/examples/magicseq.pl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        N = 123,
        magic_sequence(N, Seq),
        write(Seq),nl,nl,
        fd_statistics.


% Note: after I wrote this I realized that count/4 is
%       deprecated in version 4.1.
magic_sequence(N, Seq) :-
        length(Seq,N),
        N1 is N-1,
        domain(Seq,0,N1),

        ( for(I,0,N1),
          foreach(S,Seq),
          param(Seq) do
              count(I,Seq,#=,S)
        ),
        sum(Seq,#=,N),
        labeling([leftmost,step,down], Seq).




