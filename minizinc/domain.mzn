% 
% Global constraint domain in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cdomain.html
% """
% domain​(VARIABLES,​LOW,​UP)
% 
% Purpose
% 
% Enforce all the variables of the collection VARIABLES to take a value 
% within the interval ​[LOW,​UP]​.
% 
% Example
%     ​(​[2,​8,​2]​,​1,​9)​
% 
% The domain constraint holds since all the values 2, 8 and 2 of its 
% first argument are greater than or equal to its second argument LOW=1 
% and less than or equal to its third argument UP=9.
% 
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 3;
array[1..n] of var 1..8: x;
var int: low;
var int: up;

%
% Note: the name domain is reserved in MiniZinc
%
predicate domain_pred(array[int] of var int: variables, var int: low, var int: up) =
  forall(i in index_set(variables)) (
    variables[i] >= low
    /\
    variables[i] <= up
  )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
  cp1d(x, [2,8,2])
  /\
  low = 1
  /\
  up = 9
  /\
  domain_pred(x, low, up)
;

output
[ 
  "x: " ++ show(x) ++ "\n" ++
  "low: " ++ show(low) ++ "\n" ++
  "up: " ++ show(up) ++ "\n"

];
