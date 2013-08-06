/*

  All interval problem in B-Prolog.

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
 
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

%
% Reporting both time and backtracks
%
time2(Goal):-
        cputime(Start),
        statistics(backtracks, Backtracks1),
        call(Goal),
        statistics(backtracks, Backtracks2),
        cputime(End),
        T is (End-Start)/1000,
        Backtracks is Backtracks2 - Backtracks1,
        format('CPU time ~w seconds. Backtracks: ~d\n', [T, Backtracks]).



go :-
        N = 12,
        findall(X,time2(all_interval(N,X)),L),
        length(L, Len),
        writeln('Len:':Len),
        nl,nl.

all_interval(N,X) :-

        length(X,N),
        X :: 1..N,

        N1 is N-1,
        length(Diffs,N1),
        Diffs :: 1..N1,

        SumDistinct is ((N+1)*N) div 2,

        alldifferent(X),
        alldifferent(Diffs),
        
        foreach(K in 1..N1, Diffs[K] #= abs(X[K+1] - X[K])),
        
        % symmetry breaking
        X[1] #< X[N1],
        Diffs[1] #< Diffs[2],

        term_variables([X,Diffs], Vars),

        %
        % With largest and indomain_min the first solution is found
        % very fast with zero backtrack, but next solutions takes
        % longer.
        % This first solution has the Diffs as the sequence 1..N-1.
        %
        % labeling([ff,down], Vars),
        labeling(Vars),
        writeln(x:X),
        writeln(diffs:Diffs),
        writeln(sum_distinct:SumDistinct),
        nl.
