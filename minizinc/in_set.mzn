% 
% Global constraint in_set in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cin_set.html
% """
% in_set​(VAL,​SET)
% 
% Purpose
%
% Constraint variable VAL to belong to set SET.
% 
% Example
% ​(3,​ {1, 3})
% 
% """

% Note: This is the same (modulo syntax) as the built in "in".

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
var 1..5: val;
var set of 1..5: s;

predicate in_set(var int: val, var set of int: s) = 
   val in s
;

solve satisfy;

constraint

  s = {1,3}
  /\
  val = 3
  /\
  in_set(val, s)
;

output [
"s: " ++ show(s) ++ "\n"++
"val: " ++ show(val)
];
