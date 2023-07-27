% 
% Global constraint among_interval in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Camong_interval.html
% """
% NVAR is the number of variables of the collection VARIABLES taking a 
% value that is located within interval [LOW, UP].
% 
% Example
%     ​(3,​〈4,​5,​8,​4,​1〉,​3,​5)​
% 
% The among_interval constraint holds since we have 3 values, 
% namely 4, 5 and 4 that are situated within interval [3, 5].
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 5;
array[1..n] of var 1..8: x;
var int: nvar;
var int: low;
var int: up;

predicate among_interval(var int: nvar, array[int] of var int: x, var int: low, var int: upper)=
  nvar = sum(i in index_set(x)) (
    bool2int(
       x[i] >= low /\ x[i] <= upper
   )
  )
;

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
  cp1d([4,5,8,4,1], x)
  /\
  among_interval(nvar, x, low, up)
  /\
  low = 3 
  /\
  up = 5
;


output 
[
  "x: " ++ show(x) ++ "\n" ++
  "low: " ++ show(low) ++ "\n" ++
  "up: " ++ show(up) 
];
