% 
% Set partition problem in Minizinc.
%
% Problem formulation from
%   http://www.koalog.com/resources/samples/PartitionProblem.java.html
% """
%  This is a partition problem.
%  Given the set S = {1, 2, ..., n}, 
%  it consists in finding two sets A and B such that:
%  <ul>
%  <li>A U B = S,</li>
%  <li>|A| = |B|,</li>
%  <li>sum(A) = sum(B),</li>
%  <li>sum_squares(A) = sum_squares(B).</li>
%  </ul>
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% 
include "globals.mzn"; 

int: n = 16;
set of 1..n: S = 1..n;
int: num_sets = 2;
array[1..num_sets] of var set of S: a;
array[1..num_sets] of var 0..n*n: sums;
array[1..num_sets] of var 0..n*n*n*n: sum_squared;


%
% set_sum
% sums the elements in the set s
% 
predicate set_sum(var set of int: s, var int: the_sum) =
   the_sum = sum(i in ub(s)) (bool2int(i in s)*i)
;

predicate set_sum_squared(var set of int: s, var int: the_sum) =
   the_sum = sum(i in ub(s)) (bool2int(i in s)*i*i)
;


solve :: set_search(a, first_fail, indomain_min, complete) satisfy;
% solve maximize sums[1];

constraint
  assert(n mod 4 == 0, "n must be a multiple of 4")
;

% (
%  20080419: 
%  eclipse gives the following error
%  instantiation fault in dvar_remove_smaller(_18602{0 .. 20}, 1)
% )
constraint
   % use all the elements in S and it should be disjoint sets
   partition_set(a, S)
   /\
   forall(i in 1..num_sets) (   
     a[i] `set_sum` sums[i] 
     /\ a[i] `set_sum_squared` sum_squared[i]
   )
   /\
   forall(i in 2..num_sets) (
     card(a[i]) > 0 /\ % this is needed by eclipse
     card(a[i]) = card(a[i-1]) /\
     sums[i] = sums[i-1] 
     /\ sum_squared[i] = sum_squared[i-1] 
   )

  % symmetry breaking
  /\ 1 in a[1]

;

output [
   "a: " ++ show(a) ++ "\n" ++
   "sums: " ++ show(sums) ++ "\n" ++ 
   "sum_squared: " ++ show(sum_squared) ++ "\n" 
];

% For model seeker
% output [
%    show(set2array(fix(a[i]))) ++ ","
%   | i in 1..num_sets
% ];
