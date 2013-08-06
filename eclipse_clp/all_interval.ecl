/*

  All interval problem in ECLiPSe..
  
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
 

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(propia).


go :-
        N = 12,
        all_interval(N),
        fail.

all_interval(N) :-

        dim(X,[N]),
        X :: 1..N,

        N1 is N-1,
        dim(Diffs,[N1]),
        Diffs :: 1..N1,

        SumDistinct is ((N+1)*N) div 2,

        ic_global:alldifferent(X),
        ic_global:alldifferent(Diffs),
        
        ( for(K,1,N1), param(X,Diffs) do
              Diffs[K] #= abs(X[K+1] - X[K])
        ),
        
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
        search(Vars,0,largest,indomain_min,complete,[backtrack(Backtracks)]),
        writeln(x:X),
        writeln(diffs:Diffs),
        writeln(sum_distinct:SumDistinct),
        writeln(backtracks:Backtracks).
