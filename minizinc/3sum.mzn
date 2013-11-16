% 
% 3SUM (Three Elements That Sum To Zero) in MiniZinc.
% 
% From
% http://nathanleclaire.com/blog/2013/10/22/three-elements-that-sum-to-zero/
% """
% Given a collection of integers, return the indices of any three elements which sum to zero. 
% For instance, if you are given {-1, 6, 8, 9, 10, -100, 78, 0, 1}, you could return {0, 7, 8} 
% because -1 + 1 + 0 == 0. You can’t use the same index twice, and if there is no match you 
% should return {-1, -1, -1}.
% """

% Also see: https://en.wikipedia.org/wiki/3SUM

% Note: This model skips the last requirement, i.e. the (-1,-1,-1)) if unsat.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 9;
array[1..n] of int: nums = [-1, 6, 8, 9, 10, -100, 78, 0, 1];
% array[1..n] of int: nums = [1, 6, 8, 9, 10, 100, 78, 0, 1]; % UNSAT
int: m = 3; % The number of elements that should add to 0


% decision variables
array[1..n] of var 0..1: x;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;


constraint

   sum([nums[i]*x[i] | i in 1..n]) = 0
   /\
   sum(x) = m
;

output [
  "nums: " ++ show(nums) ++ "\n" ++
  "x   : " ++ show(x) ++ "\n" ++
  show([i | i in 1..n where fix(x[i]) = 1])
];

