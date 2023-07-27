% 
% Global constraint cardinality_atmost in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Ccardinality_atmost.html
% """
% ATMOST is the maximum number of occurrences of each value of VALUES within 
% the variables of the collection VARIABLES.
% 
% Example
% (
%  2, <2, 1, 7, 1, 2>,
%     <5, 7, 2, 9>
% )
% 
% In this example, values 5, 7, 2 and 9 occur respectively 0, 1, 2 and 0 times 
% within the collection <2, 1, 7, 1, 2>. As a consequence, the 
% cardinality_atmost constraint holds since its first argument ATMOST 
% is assigned to the maximum number of occurrences 2.

%
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 


int: n = 5;
int: m = 4;
array[1..n] of var 1..9: variables;
array[1..m] of var 1..9: values;
var 0..9: nvar;

predicate cardinality_atmost(var int: nvar, array[int] of var int: variables, array[int] of var int: values) =
   forall(i in index_set(values)) (
     sum(j in index_set(variables)) (bool2int(values[i] = variables[j])) <= nvar
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

  cp1d([2,1,7,1,2], variables)
  /\
  cp1d([5,7,2,9], values)
   /\
   nvar = 2
   /\
   cardinality_atmost(nvar, variables, values)
;

output
[
  "variables: " ++ show(variables) ++ "\n" ++
  "nvar: " ++ show(nvar) ++ "\n" ++ 
  "values: " ++ show(values)
];
