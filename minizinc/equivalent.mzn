% 
% Global constraint equivalent in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cequivalent.html
% """
% Constraint
%
%     equivalent(VAR,VARIABLES)
%
% Purpose
%
%     Let VARIABLES be a collection of 0-1 variables VAR1,VAR2. Enforce VAR=(VAR1<=>VAR2).
%
% Example
%     (1,<0,0>)
%     (0,<0,1>)
%     (0,<1,0>)
%     (1,<1,1>)
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

var 0..1: b;
array[1..2] of var 0..1: variables;

solve satisfy;


predicate equivalent(var int: v, array[int] of var int: vars) =
   forall(i, j in index_set(vars) where i < j) (
      vars[i] = vars[j]
   ) <-> v = 1
;

constraint
   equivalent(b, variables)
;

output [
  "b: " ++ show(b) ++ "\n" ++
  "variables: " ++ show(variables) ++ "\n"
];
