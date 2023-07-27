% 
% Global constraint in_interval in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cin_interval.html
% """
% in_interval​(VAR,​LOW,​UP)​
% 
% Purpose
%
% Enforce the domain variable VAR to take a value within the interval​
% [LOW, UP].
% 
% Example
%     ​(3,​2,​5)​
% 
% The in_interval constraint holds since its first argument VAR=3 is greater 
% than or equal to its second argument LOW=2 and less than or equal 
% to its third argument UP=5.
%
% """

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
var int: v;
var int: low;
var int: up;


predicate in_interval(var int: v, var int: low, var int: up) =
  v >= low /\
  v <= up

;


solve satisfy;

constraint
   v = 3
   /\
   low = 2
   /\
   up = 5
   /\
   in_interval(v, low, up)
;

output
[
  "v: " ++ show(v) ++ "\n" ++
  "low: " ++ show(low) ++ "\n" ++
  "up: " ++ show(up) ++ "\n" 
];
