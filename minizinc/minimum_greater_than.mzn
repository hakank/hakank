% 
% Global constraint minimum_greater_than in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cminimum_greater_than.html
% """
% Constraint
%
%     minimum_greater_than​(VAR1,​VAR2,​VARIABLES)​
%
% Purpose
%
%     VAR1 is the smallest value strictly greater than VAR2 of 
%     the collection of variables VARIABLES: this concretely 
%     means that there exists at least one variable of VARIABLES 
%     that takes a value strictly greater than VAR1.
%
% Example
%      (5, 3, <8, 5, 3, 8>) 
%
%     The minimum_greater_than constraint holds since value 5 
%     is the smallest value strictly greater than value 3 among 
%     values 8, 5, 3 and 8.
%
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 4;
array[1..n] of var 1..8: variables;
var 1..8: var1;
var 1..8: var2;

predicate minimum_greater_than(var int: var1, var int: var2, array[int] of var int: variables) =

   forall(i in index_set(variables)) (
     (var1 > var2 /\ variables[i] > var2) -> var1 <= variables[i]
   )  
   /\ % both var1 and var2 must exist in variables
   exists(i in index_set(variables)) (
     var1 = variables[i]
   )  
   /\
   exists(i in index_set(variables)) (
     var2 = variables[i]
   ) 
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
   var1 = 5 /\
   var2 = 3 /\
   cp1d(variables, [8,5,3,8]) /\
   var1 > var2
   /\
   minimum_greater_than(var1,var2,variables)
;


output
[
 "var1: " ++ show(var1) ++ "\n" ++
 "var2: " ++ show(var2) ++ "\n" ++
 "variables: " ++ show(variables) ++ "\n"

];
