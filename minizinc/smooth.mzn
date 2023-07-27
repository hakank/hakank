% 
% Global constraint smooth in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Csmooth.html
% """
% smooth(NCHANGE, TOLERANCE, VARIABLES)
% 
% Purpose
%
% NCHANGE is the number of times that |X Y|>TOLERANCE holds; X and Y 
% correspond to consecutive variables of the collection VARIABLES.
% 
% Example
% (1, 2, <1, 3, 4, 5, 2>)
% 
% In the example we have one change between values 5 and 2 since the 
% difference in absolute value is greater than the tolerance (i.e., |5-2|>2). 
% Consequently the NCHANGE argument is fixed to 1 and the smooth 
% constraint holds.
%
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 5;
array[1..n] of var 1..5: x;
var 1..n: nchange;
var 1..10: tolerance;

predicate smooth(var int: nchange, var int: tolerance, array[int] of var int: x) =
  nchange = sum(i in min(index_set(x))+1..max(index_set(x))) (
    bool2int(abs(x[i]-x[i-1]) > tolerance)
  ) 
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
  cp1d(x,[1,3,4,5,2])
  /\
  nchange = 1
  /\
  tolerance = 2
  /\
  smooth(nchange, tolerance, x)
;

output [
"nchange: " ++ show(nchange) ++ "\n"++
"tolerance: " ++ show(tolerance) ++ "\n"++
"x: " ++ show(x) ++ "\n"
];
