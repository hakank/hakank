% 
% Global constraint imply in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cimply.html
% """
% imply​(VAR,​VARIABLES)​
%
% Purpose
%
% Let VARIABLES be a collection of 0-1 variables VAR1,​VAR2. Enforce 
% VAR=(VAR1 -> VAR2)​.
% 
% Example
%    ​(1,​<0, 0>)​
%    ​(1,​<0, 1>)​
%    ​(0,​<1, 0>)​
%    ​(1,​<1, 1>)
%
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
array[1..2] of var bool: vars;
var bool: v; % 


predicate imply(var bool: v, array[1..2] of var bool: vars) =
  v <-> vars[1] -> vars[2]
;

solve satisfy;

constraint
   imply(v, vars)
;

output
[
  "v: " ++ show(v) ++ "\n" ++
  "vars: " ++ show(vars)
];
