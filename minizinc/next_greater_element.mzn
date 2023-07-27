% 
% Global constraint next_greater_element in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cnext_greater_element.html
% """
% Constraint
%
%     next_greater_element​(VAR1,​VAR2,​VARIABLES)​
% 
% Purpose
%
%     VAR2 is the value strictly greater than VAR1 located at the 
%     smallest possible entry of the table TABLE. In addition, the 
%     variables of the collection VARIABLES are sorted in strictly 
%     increasing order.
%
% Example
%     ​(7, 8, <3, 5, 8, 9>)
%
%     The next_greater_element constraint holds since:
%         * VAR2 is fixed to the first value 8 strictly greater than VAR1=7,
%         * The var attributes of the items of the collection VARIABLES 
%           are sorted in strictly increasing order.
% Usage
%
%     Originally introduced for modelling the fact that a nucleotide has to be consumed as soon as possible at cycle VAR2 after a given cycle VAR1.
%
% Remark
%
%     Similar to the minimum_greater_than constraint, except for the 
%     fact that the var attributes are sorted.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 4;
array[1..n] of var 1..9: variables;
var 1..ub_array(variables): var1;
var 1..ub_array(variables): var2;

predicate next_greater_element(var int: var1, 
                              var int: var2,
                              array[int] of var int: variables) =
   exists(i in index_set(variables)) (
      variables[i] > var1 /\
      variables[i] = var2 /\
      % all values before i must be less than var1, i.e.
      % var2 is the smallest possible entry
      forall(j in lb_array(variables)..i-1) (
         variables[j] < var1
      )
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 
 

solve satisfy;

constraint
   var1 = 7 /\
   var2 = 8 /\
   cp1d(variables, [3,5,8,9]) /\
   next_greater_element(var1,var2,variables)
;

output [
"var1: " ++ show(var1) ++ "\n"++
"var2: " ++ show(var2) ++ "\n"++
"variables: " ++ show(variables) ++ "\n"
];
