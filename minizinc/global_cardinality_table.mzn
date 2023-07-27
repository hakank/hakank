% 
% Global constraint global_cardinality_table in MiniZinc.
% 
% Note: The predicate global cardinality in MiniZinc's global.mzn
% assumes two arrays variables, occurrence where occurrence represents
% the values in consecutive order.
% 
% In constrast, this implementation, which I call 
% constraint global_cardinality_table is more general in that the 
% second argument is a table of values and occurrences. It is, however,
% more expensive.
% 
% See Global constraint catalog:
%  https://sofdem.github.io/gccat/gccat/Cglobal_cardinality.html
% """
% Constraint
%
%     global_cardinality​(VARIABLES,​VALUES)​
% 
% Purpose

%     Each value VALUES[i].val​(1<=i<=|VALUES|) should be taken by exactly 
%     VALUES[i].noccurrence variables of the VARIABLES collection.

% Example
%     (
%     <3,​3,​8,​6>,
%     <val-3 noccurrence-2,​val-5 noccurrence-0,​val-6 noccurrence-1>
%     )
%
%     The global_cardinality constraint holds since values 3, 5 and 6 
%     respectively occur 2, 0 and 1 times within the collection 
%     <3,​3,​8,​6> and since no constraint was specified for value 8.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 4;
int: m = 3;
array[1..n] of var 1..8: variables;
array[1..m, 1..2] of var 0..6: values;

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
   cp1d(variables, [3,3,8,6]) /\
   cp2d(values, array2d(1..m, 1..2, [
               3,2,
               5,0,
               6,1
            ]))
   /\
   global_cardinality_table(variables, values)
;


output
[
  "variables: " ++ show(variables) ++ "\n" ++
  "values: " ++ show(values) ++ "\n"
];
