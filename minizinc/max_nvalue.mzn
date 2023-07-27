% 
% Global constraint max_nvalue in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cmax_nvalue.html
% """
%     max_nvalue​(MAX,​VARIABLES)​
%
% Purpose
%
%     MAX is the maximum number of times that the same value is taken by 
%     the variables of the collection VARIABLES.
%
% Example
%     (
%     3, <
%     var-9,
%     var-1,
%     var-7,
%     var-1,
%     var-1,
%     var-6,
%     var-7,
%     var-7,
%     var-4,
%     var-9
%     >
%     )
%
%     In the example, values 1, 4, 6, 7, 9 are respectively used 
%     3, 1, 1, 3, 2 times. So the maximum number of time MAX that 
%     a same value occurs is 3. Consequently the max_nvalue 
%     constraint holds.
%
% Usage
%
%     This constraint may be used in order to replace a set of 
%     count or among constraints were one would have to generate 
%     explicitly one constraint for each potential value. Also 
%     useful for constraining the number of occurrences of the mostly 
%     used value without knowing this value in advance and without 
%     giving explicitly an upper limit on the number of occurrences 
%     of each value as it is done in the global_cardinality constraint.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 10;
array[1..n] of var 1..9: variables;
var 1..n: t_max;

solve satisfy;

predicate max_nvalue(var int: t_max, array[int] of var int: variables) =
   let {
       array[min(index_set(variables))..max(index_set(variables))] of var 0..length(variables): occ
   } in
   global_cardinality(variables, set2array(index_set(occ)), occ)
   /\
   maximum(t_max, occ)
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

constraint
   cp1d(variables, [9,1,7,1,1,6,7,7,4,9])
   /\
   t_max = 3
   /\
   max_nvalue(t_max, variables) 
;

output
[
  "variables:" ++ show(variables) ++ "\n" ++
  "t_max:" ++ show(t_max) ++ "\n"
];
