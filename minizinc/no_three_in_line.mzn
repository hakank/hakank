% 
% No three in line problem in MiniZinc.
% 
% From
% http://en.wikipedia.org/wiki/No-three-in-line_problem
% """
% In mathematics, in the area of discrete geometry, the no-three-in-line-problem, 
% introduced by Henry Dudeney in 1917, asks for the maximum number of points 
% that can be placed in the n × n grid so that no three points are collinear. 
% This number is at most 2n, since if 2n + 1 points are placed in the grid some 
% row will contain three points.
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 
% include "gecode.mzn"; % for afc_min
int: n = 11;
int: m = 3;
array[1..n, 1..n] of var 0..1: x;
var 0..2*n: tot;


% solve satisfy;
solve :: int_search(
        [x[i,j] | i,j in 1..n] ++ [tot], 
        first_fail, % size_afc_min, % smallest, % first_fail, % occurrence, 
        indomain_max, 
        complete) 
    % satisfy;
    maximize tot;

constraint
   tot = sum([x[i,j] | i,j in 1..n])
   % /\ tot = 2*n
   /\
   forall(i in 1..n) (
      sum([x[a,i] | a in 1..n]) < m /\
      sum([x[i,a] | a in 1..n]) < m
   )
;


output
[
  "x: " ++ show(x) ++ "\n" ++
  "tot: " ++ show(tot) ++ "\n" 
];