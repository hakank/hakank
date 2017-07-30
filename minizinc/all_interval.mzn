% 
% All interval problem in MiniZinc
% 
% CSPLib problem number 7
% http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
% """
% Given the twelve standard pitch-classes (c, c%, d, ...), represented by 
% numbers 0,1,...,11, find a series in which each pitch-class occurs exactly 
% once and in which the musical intervals between neighbouring notes cover 
% the full set of intervals from the minor second (1 semitone) to the major 
% seventh (11 semitones). That is, for each of the intervals, there is a 
% pair of neigbhouring pitch-classes in the series, between which this 
% interval appears. The problem of finding such a series can be easily 
% formulated as an instance of a more general arithmetic problem on Z_n, 
% the set of integer residues modulo n. Given n in N, find a vector 
% s = (s_1, ..., s_n), such that (i) s is a permutation of 
% Z_n = {0,1,...,n-1}; and (ii) the interval vector 
% v = (|s_2-s_1|, |s_3-s_2|, ... |s_n-s_{n-1}|) is a permutation of 
% Z_n-{0} = {1,2,...,n-1}. A vector v satisfying these conditions is 
% called an all-interval series of size n; the problem of finding such 
% a series is the all-interval series problem of size n. We may also be 
% interested in finding all possible series of a given size. 
% """

%
% Compare with the Gecode/R model http://www.hakank.org/gecode_r/all_interval.rb
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% (Very old note: For n = 12, all 1328 solution (no symmetry breaking) took about 25 
%       seconds.
%       With symmetry breaking: 463 solutions in 15 seconds.)
% (Now Gecode/fz takes about 2 seconds for generating all solutions withoutsymmetry breaking,
%  and 0.8s with symmetry breaking.)

include "globals.mzn";

% int: n = 12;
int: n; 
% array[1..n] of var 1..n: x;
array[1..n] of var 1..n: x;
array[1..n-1] of var 1..n-1: diffs;
int: sum_distinct = ((n+1)*n) div 2;

% max_regret seems to be quite good....
solve :: int_search(x ++diffs, max_regret, indomain_split, complete) satisfy;

constraint      
  all_different(diffs) :: domain
  /\  
  all_different(x) :: domain
  /\ 
  forall(k in 1..n-1) (
      diffs[k] = abs(x[k+1] - x[k])
  )
  /\ % symmetry breaking
  x[1] < x[n-1]
  /\
  diffs[1] < diffs[2]
  % /\ increasing(diffs) % for just one solution
;


output [
       show(x) ++ "," , " ", show(sum_distinct), " diffs: ", show(diffs)
]
