% 
% n-artitioning in MiniZinc.
% 
% http://en.wikipedia.org/wiki/3-partition_problem
% """
% The 3-partition problem is an NP-complete problem in computer science. The problem is to decide 
% whether a given multiset of integers can be partitioned into triples that all have the same sum. 
% More precisely, given a multiset S of n = 3 m positive integers, can S be partitioned into m 
% subsets S1, S2, ..., Sm such that the sum of the numbers in each subset is equal? The subsets 
% S1, S2, ..., Sm must form a partition of S in the sense that they are disjoint and they cover S. 
% Let B denote the (desired) sum of each subset Si, or equivalently, let the total sum of the 
% numbers in S be m B. The 3-partition problem remains NP-complete when every integer in S is 
% strictly between B/4 and B/2. In this case, each subset Si is forced to consist of exactly three 
% elements (a triple).
% """
%
% Cf number_partitioning.mzn etc
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n; % number of sets
int: m; % the cardinality of the sets, m=3 -> triples
set of int: nums; % the numbers to partition

array[1..n] of var set of nums: a;
array[1..n] of var int: a_sum;

%
% set_sum
% sums the elements in the set s
% 
% (from set_partition.mzn)
% 
predicate set_sum(var set of int: s, var int: the_sum) =
   the_sum = sum(i in ub(s)) (bool2int(i in s)*i)
;


% fz 1.2.1 and fd have problem with this int_search hint:
% solve :: int_search(a_sum, "first_fail", "indomain_min", "complete") satisfy;
% solve :: set_search(a, "first_fail", "indomain_min", "complete") satisfy;
solve satisfy;

constraint
   assert (card(nums) = n*m, "card(nums) must be = n*m")
   /\
   forall(i in 1..n) (
      a_sum[i] >= 0
   )
   /\
   partition_set(a, nums) /\
   forall(i in 1..n) (
      set_sum(a[i], a_sum[i]) /\
      card(a[i]) = m

   )
   /\
   forall(i in 2..n) (
     a_sum[i] = a_sum[i-1] 
     % /\ a[i-1] < a[i] % set comparisons don't work in minizinc 0.7.1. It does in 0.8.
   )
;

%
% data
%
n = 5;
m = 3;
% nums = {4,5,6,7,8};
% nums = {2,3,6,7,9,10,12,14,23,63,48,64,88,4};
nums = 1..n*m;


output
[
  "a: " ++ show(a) ++ "\n" ++
  "a_sum: " ++ show(a_sum) ++ "\n"
];
