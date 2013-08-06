%
% All interval problem in MiniZinc.
% 
% Different approaches inspired by
% http://www.dis.uniroma1.it/~tmancini/index.php?currItem=research.publications.webappendices.csplib2x.problemDetails&problemid=007

% Problem description
% Given the twelve standard pitch-classes (c, c#, d, ...), represented by numbers 0,1,...,11, this problem amounts to find a series in which each pitch-class occurs exactly once and in which the musical intervals between neighboring notes cover the full set of intervals from the minor second (1 semitone) to the major seventh (11 semitones). That is, for each of the intervals, there is a pair of neighboring pitch-classes in the series, between which this interval appears.

% We consider a generalization of this problem in which the set of numbers is the range from 0 to n-1, for any given positive 'n'. In particular, given such 'n', the problem amounts to find a vector s = (s1, ..., sn) that is a permutation of {0, 1,..., n-1} and such that the interval vector v = (|s2 - s1|, |s3 - s2|, ..., |sn - s(n-1)|) is a permutation of {1, 2,..., n-1}.
%
% Problem input
%
%     * n, the number of pitch classes 
%
% Search space
% The set of permutations of integer range [0..n-1]
%
% Constraints
%
%     * C1: Each pitch class occurs exactly once
%     * C2: Differences between neighbouring notes are all different

%
% Also see
%   http://www.hakank.org/minizinc/all_interval1.mzn
%   http://www.hakank.org/minizinc/all_interval2.mzn
%   http://www.hakank.org/minizinc/all_interval3.mzn
%   http://www.hakank.org/minizinc/all_interval4.mzn
%   http://www.hakank.org/minizinc/all_interval5.mzn
%   http://www.hakank.org/minizinc/all_interval6.mzn
%

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc



include "globals.mzn";
int: n = 12;
set of int: classes = 0..n-1;

% Search space: The set of permutations of integer range [0..n-1]
array[classes] of var classes: series;

solve :: int_search(series, first_fail, indomain_min, complete) satisfy;

constraint
%    all_different(series) /\

   % C1: Each pitch class occurs exactly once
   forall(i,j in classes where i != j) (
     series[i] != series[j]
   )
   /\
   % C2: Differences between neighbouring notes are all different
   forall(i,j in classes where j < n-1 /\ i < n-1 /\ i != j)  (
     abs(series[i+1] - series[i]) != abs(series[j+1] - series[j])
   )
;

output
[  show(series)
];