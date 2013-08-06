/*

  All interval problem in SICStus Prolog.

  CSPLib problem number 7
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
  """
  Given the twelve standard pitch-classes (c, c , d, ...), represented by 
  numbers 0,1,...,11, find a series in which each pitch-class occurs exactly 
  once and in which the musical intervals between neighbouring notes cover 
  the full set of intervals from the minor second (1 semitone) to the major 
  seventh (11 semitones). That is, for each of the intervals, there is a 
  pair of neigbhouring pitch-classes in the series, between which this 
  interval appears. The problem of finding such a series can be easily 
  formulated as an instance of a more general arithmetic problem on Z_n, 
  the set of integer residues modulo n. Given n in N, find a vector 
  s = (s_1, ..., s_n), such that (i) s is a permutation of 
  Z_n = {0,1,...,n-1}; and (ii) the interval vector 
  v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of 
  Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is 
  called an all-interval series of size n; the problem of finding such 
  a series is the all-interval series problem of size n. We may also be 
  interested in finding all possible series of a given size. 
  """

 Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/all_interval.mzn
  * Comet   : http://www.hakank.org/comet/all_interval.co 
  * Gecode/R: http://www.hakank.org/gecode_r/all_interval.rb
  * ECLiPSe : http://www.hakank.org/eclipse/all_interval.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 12,
        all_interval(N),
        fd_statistics,
        fail.


all_interval(N) :-

        length(Xs,N),
        domain(Xs, 1, N),

        N1 is N-1,
        length(Diffs,N1),
        domain(Diffs,1,N1),

        SumDistinct is ((N+1)*N) // 2,
        % sum(Xs,#=,SumDistinct),

        all_different(Xs),
        all_different(Diffs),
        
        ( fromto(Xs, [This,Next | Rest], [Next|Rest],[_]),
          foreach(Diff,Diffs)
        do
          Diff #= abs(This - Next)
        ),
        
        % symmetry breaking
        element(1,Xs,XsFirst),
        element(N1,Xs,XsLast),
        XsFirst #< XsLast,
        Diffs = [Diffs1,Diffs2|_],
        Diffs1 #< Diffs2,

        %
        % With labeling ff the first solution is found
        % very fast with zero backtrack, but next solutions takes
        % longer.
        % This first solution has the Diffs as the sequence 1..N-1.
        %
        append(Diffs,Xs, Vars),
        labeling([ff], Vars),

        write(x:Xs),nl,
        write(diffs:Diffs),nl,
        write(sum_distinct:SumDistinct),nl,nl.

