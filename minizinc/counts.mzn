% 
% Global constraint counts (with relops) in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Ccounts.html
% """
% counts​(VALUES,​VARIABLES,​RELOP,​LIMIT)
% 
% Let N be the number of variables of the VARIABLES collection assigned to 
% a value of the VALUES collection. Enforce condition N RELOP LIMIT to hold.
% 
% Example
% (
%    <1,​3,​4,​9>,​
%    <4,​5,​5,​4,​1,​5>,​=,​3
%    )
% 
% Values 1, 3, 4 and 9 of the VALUES collection are assigned to 3 items of the 
% VARIABLES=<4,​5,​5,​4,​1,​5> collection. The counts constraint holds 
% since this number is in fact equal (RELOP is set to =) to the last argument 
% of the counts constraint.

% """
% 

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: m = 4;
int: n = 6;
array[1..n] of var 1..5: variables;
array[1..m] of var 1..9: values;
var int: limitx;


solve satisfy;

%
% Since MiniZinc don't handle function variables we use the following 
% hack where t is the type of comparison operator.
% t:
%  - 2 :  a <  b
%  - 1 :  a <= b
%    0 :  a  = b
%    1 :  a >= b
%    2 :  a >  b
% else : a != b
%
predicate cmp(var int: a, var int: b, int: t) = 
   if     t = -2 then
     a < b
   elseif t = -1 then
     a <= b 
   elseif t =  0 then
     a = b
   elseif t =  1 then
     a >= b
   elseif t =  2 then
     a > b
   else 
     a != b
   endif
;

%
% counts(VARIABLES, VALUES, RELOP, LIMIT)
%
predicate counts(array[int] of var int: variables, array[int] of var int: values, int: relop, var int: limit) =

     cmp(sum(i in index_set(variables)) (
        sum(j in index_set(values)) (
            bool2int(variables[i] = values[j])
       )
     ), limit, relop)
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 


constraint
   cp1d([1,3,4,9], values)
   /\
   cp1d([4,5,5,4,1,5], variables)
   /\
   limitx = 3
   /\
   counts(values, variables, 0, limitx)



;


output
[
  "values: " ++ show(values) ++ "\n" ++
  "variables: " ++ show(variables) ++ "\n" ++
  "limitx: " ++ show(limitx) ++ "\n"
];
