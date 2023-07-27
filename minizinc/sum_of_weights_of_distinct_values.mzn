% 
% Global constraint sum_of_weights_of_distinct_values in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Csum_of_weights_of_distinct_values.html
% """
% Constraint
%
%     sum_of_weights_of_distinct_values​(VARIABLES,​VALUES,​COST)​
%
% Purpose
%
%     All variables of the VARIABLES collection take a value in the VALUES 
%     collection. In addition COST is the sum of the weight attributes 
%     associated with the distinct values taken by the variables of VARIABLES.
%
% Example
%     (
%     <1,6,1>,
%     <val-1 weight-5, val-2 weight-3, val-6 weight-7>, 12
%     )
%
%     The sum_of_weights_of_distinct_values constraint holds since its 
%     last argument COST=12 is equal to the sum 5+7 of the weights of 
%     the values 1 and 6 that occur within the <1,6,1> collection.
%
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 3;
int: m = 3;
array[1..n] of var 1..6: variables;
array[1..m, 1..2] of var 1..7: values;
var 1..1000: cost;

predicate sum_of_weights_of_distinct_values(array[int] of var int: variables,
                                            array[int,int] of var int: values,
                                            var int: cost) =
   cost = sum(i in index_set_1of2(values)) (
              values[i,2]*bool2int(exists(j in index_set(variables)) (
                 variables[j] = values[i,1]
              ))
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
   cp1d(variables, [1,6,1]) /\
   cost = 12 /\
   cp2d(values, array2d(1..m, 1..2, [
                     1,5,
                     2,3,
                     6,7
                    ]))

   /\
   sum_of_weights_of_distinct_values(variables, values, cost)  
;


output [
"variables: " ++ show(variables) ++ "\n"++
"values: " ++ show(values) ++ "\n"++
"cost: " ++ show(cost) ++ "\n"
];
