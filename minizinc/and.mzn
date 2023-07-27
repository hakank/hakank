% 
% Global constraint arith in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cand.html
% """
% Let VARIABLES be a collection of 0-1 variables 
% VAR1, VAR2, ..., VARn (n>=2). Enforce VAR=VAR1/\VAR2../\VARn.
% 
% Example
% (0, <0, 0>)
% (0, <0, 1>)​
% (0, <1, 0>)
% (1, <1, 1>)
% (0, <1, 0, 1>)
% """ 

% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 3;
array[1..n] of var 0..1: x;
var 0..1: y;

solve satisfy;

predicate and(array[int] of var int: x, var 0..1: y) =
   y = 1 <-> n = sum(i in index_set(x)) (
     bool2int(x[i] = 1) 
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",  
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
; 


constraint
   cp1d([1, 1, _],x)
   /\
   and(x, y)
  % /\
  % y = 0

;

output
[
  "x:" ++ show(x) ++ "\n" ++
  "y:" ++ show(y) ++ "\n"
];
