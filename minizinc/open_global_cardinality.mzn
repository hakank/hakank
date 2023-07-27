% 
% Global constraint open_global_cardinality in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Copen_global_cardinality.html
% """
% Constraint
%
%     open_global_cardinality​(S,​VARIABLES,​VALUES)​
%
% Purpose
%
%     Each value VALUES[i].val(1<=i<=|VALUES|)​should be taken by exactly 
%     VALUES[i].noccurrence variables of the VARIABLES collection for which 
%     the corresponding position belongs to the set S.
%
% Example
%     (
%     ​{2,​3,​4},
%     <3,​3,​8,​6>,
%     <val−3 noccurrence−1,​val−5 noccurrence−0,​val−6 noccurrence−1>
%     )
%
%     The open_global_cardinality constraint holds since:
%     * Values 3, 5 and 6 respectively occur 1, 0 and 1 times within 
%       the collection <3,​3,​8,​6> (the first item 3 of <3,​3,​8,​6> 
%       is ignored since value 1 does not belong to the first argument 
%       S=​{2,​3,​4}​ of the open_global_cardinality constraint).
%     * No constraint was specified for value 8.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 4;
array[1..n] of var 1..8: variables;
var set of 1..n: s;
int: m = 3;
array[1..m, 1..2] of var 0..6: values;

predicate open_global_cardinality(var set of int: s, 
                                  array[int] of var int: variables,
                                  array[int,int] of var int: values) =

  forall(i in index_set_1of2(values)) (
          values[i,2] = sum(j in index_set(variables)) (
                             bool2int( 
                                j in s /\
                                variables[j] = values[i,1]
                             )
                        )
   )
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

solve satisfy;

constraint
   s = {2,3,4} /\
   cp1d(variables, [3,3,8,6]) /\
   cp2d(values, array2d(1..m, 1..2, [
                       3,1,
                       5,0,
                       6,1
                    ])) /\

    open_global_cardinality(s, variables, values)
;


output
[
  "variables: " ++ show(variables) ++ "\n" ++
  "values: " ++ show(values) ++ "\n" ++
  "s: " ++ show(s) ++ "\n"
];
