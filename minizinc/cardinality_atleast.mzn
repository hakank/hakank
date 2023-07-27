% 
% Global constraint cardinality_atleast in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Ccardinality_atleast.html
% """
% ATLEAST is the minimum number of time that a value of VALUES is taken by the 
% variables of the collection VARIABLES.
% 
% Example
% (1, <3, 3, 8>, <3, 8>)
% 
% In this example, values 3 and 8 are respectively used 2, and 1 times. 
% The cardinality_atleast constraint holds since its first argument ATLEAST = 1 
% is assigned to the minimum number of time that values 3 and 8 occur in the 
% collection <3, 3, 8>.

% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 


int: n = 3;
int: m = 2;
array[1..n] of var 1..8: variables;
array[1..m] of var 1..8: values;
var 0..8: nvar;

predicate cardinality_atleast(var int: nvar, array[int] of var int: variables, array[int] of var int: values) =
   forall(i in index_set(values)) (
     sum(j in index_set(variables)) (bool2int(values[i] = variables[j])) >= nvar
   )

;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
        x[i] = y[i]
    )
  )
; 


solve satisfy;

constraint

   cp1d([3,3,8],variables)
   /\
   cp1d([3,8], values)
   /\
   nvar = 1
   /\
   cardinality_atleast(nvar, variables, values)
;

output
[
  "variables: " ++ show(variables) ++ "\n" ++
  "nvar: " ++ show(nvar) ++ "\n" ++ 
  "values: " ++ show(values)
];
