/*

  All interval problem in AMPL+CP.

  CSPLib problem number 7
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob007/index.html
  """
  Given the twelve standard pitch-classes (c, c%, d, ...), represented by 
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
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

var x{1..n} integer >= 1 <= n integer;
var diffs{1..n-1} integer >= 1 <= n-1 integer;

#
# constraints
#
s.t. c1: alldiff{i in 1..n-1} (diffs[i]);
s.t. c2: alldiff{i in 1..n} (x[i]);

s.t. c3{k in 1..n-1}: diffs[k] = abs(x[k+1]-x[k]);

# symmetry breaking
s.t. c4: x[1] < x[n-1];
s.t. c5: diffs[1] < diffs[2];


data;  

param n := 18;
printf "\nn: %d\n", n;

option solver gecode;
option gecode_options 'var_branching=size_min val_branching=min outlev=1 outfreq=1';

solve;

printf "x    : ";
for{i in 1..n} { printf "%2d ", x[i]; }

printf "\ndiffs  : ";
for{i in 1..n-1} { printf "%2d ", diffs[i]; }
printf "\n\n";
