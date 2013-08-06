%
% Simple puzzle in Minizinc
%
% Problem statement in 
%   http://www.thescripts.com/forum/thread42216.html
% """
% The puzzle: a 4 x 4 grid. The rows are summed (and given), the
% cols are summed (and given), and the two diagonals are summed,
% and given. In addition, 4 clues are given, but none of the 4 are in
% the same row or col.
%
% Example from today's paper:...solution time is 8 minutes, 1 second,
% so they say.
%
% The set of allowable numbers is 1 thru 9
%
% Rows:
% 3 + B + C + D = 22
% E + F + 8 + H = 26
% I + J + K + 8 = 31
% M + 7 + O + P = 25
%
% Col sums:
% 24, 18, 31, 31
%
% Diag sums:
% 3 + F + K + P = 24
% M + J + 8 + D = 24
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


int: n = 4;
array[1..n] of int: rowsums;
array[1..n] of int: colsums;
array[1..n div 2] of int: diagsums;
array[1..n, 1..n] of int: clues;

array[1..n, 1..n] of var 1..9: x;

solve satisfy;

constraint
  forall(i in 1..n, j in 1..n where clues[i,j] > 0) (
     clues[i,j] = x[i,j]
  )
  /\
  forall(i in 1..n) (
     sum(j in 1..n) (x[i,j]) = rowsums[i]
  )
  /\
  forall(j in 1..n) (
   sum(i in 1..n) (x[i,j]) = colsums[j]
  )
  /\
  sum(i in 1..n) (x[i,i]) = diagsums[1]
  /\
  sum(i in 1..n) (x[i,n-i+1]) = diagsums[2]
;

rowsums = [22, 26, 31, 25];
colsums = [24, 18, 31, 31];
diagsums = [24, 24];


%
% the clues 
%
clues =  
[|5, 0, 0, 0,
 |0, 0, 8, 0,
 |0, 0, 0, 8,
 |0, 7, 0, 0
|];


output [
   if j = 1 then "\n" else " " endif ++
   show(x[i,j])
   | i,j in 1..n
] ++ ["\n"];
