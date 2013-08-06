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
% {S} = {A, B, C, D, E, F}, where:
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
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 


% One solution: S = {2,4,6} (i.e. {B,D,F})
int: n = 7;
int: m = 6;
array[1..m] of set of int: s = 
   [
      {1, 4, 7},    % A, 1
      {1, 4},       % B, 2
      {4, 5, 7},    % C, 3
      {3, 5, 6},    % D, 4
      {2, 3, 6, 7}, % E, 5
      {2, 7}        % F, 6
  ];


% From Pearls of Discrete Mathematics, page 193
% (There are two solutions:  S = {1,2,3} or S = {1,4}
% int: n = 5;
% int: m = 4;
% array[1..m] of set of int: s = 
%    [
%       {1},      % A, 1
%       {2},      % B, 2
%       {3,4,5},  % C, 3
%       {2,3,4,5},% D, 4
%   ];


array[1..m] of var 0..1: x;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;


constraint
   % ensure that all numbers 1..n are covered by exactly 
   % one choosen set
   forall(i in 1..n) (

      % exactly one
      sum(j in 1..m) ( x[j]*bool2int(i in s[j])) = 1

      %  use this for the at most 1 version
      % sum(j in 1..m) ( x[j]*bool2int(i in s[j])) <= 1
   )
;


%
% presentation
%
output [
  "x" ++ show(x) ++ "\n"
] ++ 
[
  if fix(x[i]) = 1 then show(i) ++ ": " ++ show(s[i]) ++ "\n" else "" endif
  | i in 1..m
]
++
["\n"]
;

