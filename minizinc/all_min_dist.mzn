% 
% Global constraint all_min_dist in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Call_min_dist.html
% """
% Enforce for each pair (vari, varj)​ of distinct variables of the 
% collection VARIABLES that 
% |vari - varj| >= MINDIST.
% 
% Example
%  (2, <5, 1, 9, 3>)
% 
% The all_min_dist constraint holds since the following expressions 
% |5-1|, |5-9|, |5-3|, |1-9|, |1-3|, |9-3| are all greater than or equal 
% to the first argument MINDIST = 2 of the all_min_dist constraint.
% """

% 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 4;
array[1..n] of var 1..9: x;
var 0..9: c;

%
% all_min_dist(mindist, variables)
%
predicate all_min_dist(var int: mindist, array[int] of var int: variables) =
  forall(i, j in index_set(variables) where i != j) (
    abs(variables[i]-variables[j]) >= mindist
  )
  /\
  mindist > 0
;

% copy 
predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
        x[i] = y[i]
    )
  )
; 


solve satisfy;

constraint
  %cp1d([5,1,9,3], x)
  %/\
  all_min_dist(c, x)
  /\
  c = 2

;


output [ 
  "c: " ++ show(c) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" 
];
