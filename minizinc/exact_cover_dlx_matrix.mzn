% 
% Exact cover example in MiniZinc.
% 
% This is the example given in
% http://en.wikipedia.org/wiki/Exact_cover
% and
% http://en.wikipedia.org/wiki/Knuth%27s_Algorithm_X#Example
% """
% For example, consider the exact cover problem specified by the 
% universe U = {1, 2, 3, 4, 5, 6, 7} and the collection of sets 
% \mathcal{S} = {A, B, C, D, E, F}, where:
% 
%         A = {1, 4, 7};
%         B = {1, 4};
%         C = {4, 5, 7};
%         D = {3, 5, 6};
%         E = {2, 3, 6, 7}; and
%         F = {2, 7}.
% 
% This problem is represented by the matrix:
% 
%      	        1 	2 	3 	4 	5 	6 	7
%     A 	1 	0 	0 	1 	0 	0 	1
%     B 	1 	0 	0 	1 	0 	0 	0
%     C 	0 	0 	0 	1 	1 	0 	1
%     D 	0 	0 	1 	0 	1 	1 	0
%     E 	0 	1 	1 	0 	0 	1 	1
%     F 	0 	1 	0 	0 	0 	0 	1
% 
% Algorithm X with Knuth's suggested heuristic for selecting columns 
% solves this problem as follows:
% ...
% """
% 
% This model use a matrix representation instead of sets
% that where used in 
%    http://www.hakank.org/minizinc/exact_cover_dlx.mzn

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 7; % numbers used 1..n
int: m = 6; % number of sets

array[1..m, 1..n] of int: mat = array2d(1..m, 1..n, 
[
   % 1 2 3 4 5 6 7 
     1,0,0,1,0,0,1, % A {1, 4, 7}
     1,0,0,1,0,0,0, % B {1, 4}
     0,0,0,1,1,0,1, % C {4, 5, 7}
     0,0,1,0,1,1,0, % D {3, 5, 6}
     0,1,1,0,0,1,1, % E {2, 3, 6, 7}
     0,1,0,0,0,0,1, % F {2, 7}
]);

array[1..m] of var 0..1: x;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;


constraint
   % ensure that all numbers 1..n are covered by exactly 
   % one choosen set
   forall(j in 1..n) (
      sum(i in 1..m) (x[i]*mat[i,j]) = 1
   )
;


%
% presentation
%
output [
  "x" ++ show(x) ++ "\n"
] ++ 
%[
%  if fix(x[i,j]) = 1 then show(i) ++ ": " ++ show(s[i]) ++ "\n" else "" endif
%  | i in 1..m, j in 1..n
%]
% ++
["\n"]
;

