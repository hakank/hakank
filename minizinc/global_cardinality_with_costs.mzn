% 
% Global constraint in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cglobal_cardinality_with_costs.html
% """
% Constraint
%
%     global_cardinality_with_costs​(VARIABLES,​VALUES,​MATRIX,​COST)​
% ...
% Purpose
%
%     Each value VALUES[i].val should be taken by exactly 
%     VALUES[i].noccurrence variables of the VARIABLES collection. 
%     In addition the COST of an assignment is equal to the sum of 
%     the elementary costs associated with the fact that we assign 
%     the ith variable of the VARIABLES collection to the jth value 
%     of the VALUES collection. These elementary costs are given 
%     by the MATRIX collection.
%
% Example
%     (
%     <3, 3, 3, 6>,
%     <val-3 noccurrence-3, val-5 noccurrence-0, val-6 noccurrence-1>,
%     <
%     i-1 j-1 c-4,​
%     i-1 j-2 c-1,​
%     i-1 j-3 c-7,​
%     i-2 j-1 c-1,​
%     i-2 j-2 c-0,​
%     i-2 j-3 c-8,​
%     i-3 j-1 c-3,​
%     i-3 j-2 c-2,​
%     i-3 j-3 c-1,​
%     i-4 j-1 c-0,​
%     i-4 j-2 c-0,​
%     i-4 j-3 c-6
%     >,​14
%     )
%
%     The global_cardinality_with_costs constraint holds since:
%     * Values 3, 5 and 6 respectively occur 3, 0 and 1 times within the 
%       collection <3, 3, 3, 6>.
%     * The COST argument corresponds to the sum of the costs 
%       respectively associated with the first, second, third and 
%       fourth items of 〈3,​3,​3,​6〉, namely 4, 1, 3 and 6.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 4; % number of variables
int: m = 3; % number of values
array[1..n] of var 1..16: variables;
array[1..m, 1..2] of var 0..16: values;
array[1..n, 1..m] of var 0..10: cost_matrix;
var 0..1000: cost;

predicate global_cardinality_with_costs(array[int] of var int: variables,
                                        array[int,int] of var int: values,
                                        array[int,int] of var int: cost_matrix,
                                        var int: cost) =

    % global cardinality ...
    forall(j in index_set_1of2(values)) (
        values[j,2] = sum(i in index_set(variables)) (
                         bool2int(variables[i] = values[j,1])
                      )
    )
    /\ % ... with costs
    cost = sum(j in index_set_1of2(values)) (
              sum(i in index_set(variables)) (
                 cost_matrix[i,j]*bool2int(variables[i] = values[j,1])
              )
           ) 
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
predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint

    cp1d(variables, [3,3,3,6])
    /\
    cp2d(values, array2d(1..m, 1..2, [
                     3,3,
                     5,0,
                     6,1
                     ]))

    /\
    cp2d(cost_matrix, array2d(1..n, 1..m, [
                          4,1,7,
                          1,0,8,
                          3,2,1,
                          0,0,6
                         ]))
    /\
    global_cardinality_with_costs(variables,values, cost_matrix, cost)
;

output
[
  "variables: " ++ show(variables) ++ "\n" ++
  "values: " ++ show(values) ++ "\n" ++ 
  "cost: " ++ show(cost) ++ "\n" 
];
