% 
% Global constraint same in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Csame.html
% """
% The variables of the VARIABLES2 collection correspond to the variables of the VARIABLES1 
% collection according to a permutation.
% 
% Example
%     (
%     <1,9,1,5,2,1>,
%     <9,1,1,1,2,5>
%     )
% 
% The same constraint holds since values 1, 2, 5 and 9 have the same number of 
% occurrences within both collections <1,9,1,5,2,1> and <9,1,1,1,2,5>. 
% Figure 4.240.1 illustrates this correspondence.
% """
% 

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 6;
array[1..n] of var 1..9: variables1;
array[1..n] of var 1..9: variables2;


%
% same(VARIABLES1, VARIABLES2)
%
predicate same(array[int] of var int: variables1, array[int] of var int: variables2) =
   let {
      array[lb_array(variables1)..ub_array(variables1)] of var 0..ub_array(variables1): gcc1,
      array[lb_array(variables2)..ub_array(variables2)] of var 0..ub_array(variables2): gcc2,

   }
   in
   global_cardinality(variables1, set2array(index_set(gcc1)), gcc1) 
   /\
   global_cardinality(variables2, set2array(index_set(gcc2)), gcc2) 
   /\
   cp1d(gcc1, gcc2)
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint

  cp1d(variables1, [1,9,1,5,2,1])
  /\
  cp1d(variables2, [9,1,1,2,5,1])
  /\
  same(variables1,variables2)

;

output
[
  "variables1: " ++ show(variables1) ++ "\n" ++
  "variables2: " ++ show(variables2) ++ "\n"
];
