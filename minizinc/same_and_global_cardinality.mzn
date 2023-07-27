% 
% Global constraint same_and_global_cardinality in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Csame_and_global_cardinality.html
% """
% Constraint
%
%     same_and_global_cardinality(VARIABLES1,VARIABLES2,VALUES)
% 
% Purpose
%
%     The variables of the VARIABLES2 collection correspond to the variables 
%     of the VARIABLES1 collection according to a permutation. In addition, 
%     each value VALUES[i].val (1i|VALUES|) should be taken by 
%     exactly VALUES​[i].noccurrence variables of the VARIABLES1 collection.
%
% Example
%     (
%     <1,9,1,5,2,1>,
%     <9,1,1,1,2,5>,
%     <
%     val-1 noccurrence-3,
%     val-2 noccurrence-1,
%     val-5 noccurrence-1,
%     val-7 noccurrence-0,
%     val-9 noccurrence-1
%     >
%     )
%
%     The same_and_global_cardinality constraint holds since:
%     * The values 1, 9, 1, 5, 2, 1 assigned to VARIABLES1 correspond 
%       to a permutation of the values 9, 1, 1, 1, 2, 5 assigned to VARIABLES2.
%     * The values 1, 2, 5, 7 and 6 are respectively used 3, 1, 1, 0 and 1 
%       times.
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 6;
int: m = 5;
array[1..n] of var 1..9: variables1;
array[1..n] of var 1..9: variables2;
array[1..m, 1..2] of var 0..9: values ;

solve satisfy;

%
% See http://www.hakank.org/minizinc/same.mzn
%
predicate same(array[int] of var int: variables1, array[int] of var int: variables2) =
   let {
      array[lb_array(variables1)..ub_array(variables1)] of var 0..ub_array(variables1): gcc1,
      array[lb_array(variables2)..ub_array(variables2)] of var 0..ub_array(variables2): gcc2,

   }
   in
   global_cardinality(variables1, set2array(index_set(gcc1)), gcc1) 
   /\
   global_cardinality(variables1, set2array(index_set(gcc2)), gcc2) 
   /\
   cp1d(gcc1, gcc2)
;

%
% See http://www.hakank.org/minizinc/global_cardinality_table.mzn
%
predicate global_cardinality_table(array[int] of var int: variables,
                                   array[int,int] of var int: values) =

  forall(i in index_set_1of2(values)) (
          values[i,2] = sum(j in index_set(variables)) (
                             bool2int( 
                                variables[j] = values[i,1]
                             )
                        )
   )
;

predicate same_and_global_cardinality(array[int] of var int: variables1,
                                      array[int] of var int: variables2,
                                      array[int,int] of var int: values) =
   same(variables1, variables2) 
   /\
   global_cardinality_table(variables1, values)
   /\
   global_cardinality_table(variables2, values)
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 

constraint
  cp1d(variables1, [1,9,1,5,2,1]) /\
  cp1d(variables2, [9,1,1,1,2,5]) /\
  cp2d(values, array2d(1..m, 1..2, [
                         1,3,
                         2,1,
                         5,1,
                         7,0,
                         9,1
                        ]))
  /\
  same_and_global_cardinality(variables1,variables2,values)
;


output
[
  "variables1: " ++ show(variables1) ++ "\n" ++
  "variables2: " ++ show(variables2) ++ "\n" ++
  "values: " ++ show(values) ++ "\n" 
];
