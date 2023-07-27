% 
% Global constraint open_atmost in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Copen_atmost.html
% """
% Constraint
%
%     open_atmost(S,N,VARIABLES,VALUE)
%
% Purpose
%
%     Let V be the variables of the collection VARIABLES for which the 
%     corresponding position belongs to the set S. At most N variables 
%     of V are assigned to value VALUE.
%
% Example
%     ({2,3,4},1,<2,2,4,5>,2)
%
%     The open_atmost constraint holds since, within the last three 
%     (i.e., S={2,3,4}) values of the collection <2,2,4,5>, 
%     at most N=1 value is equal to value VALUE=2.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: num = 4;
array[1..num] of var 1..5: variables;
var set of 1..num: s;
var 1..num: n;
var 1..num: value;

predicate open_atmost(var set of int: s,
                       var int: n,
                       array[int] of var int: variables,
                       var int: value) =
 
   sum(i in index_set(variables)) ( 
        bool2int(
             i in s /\
             variables[i] == value
        ) 
    ) <= n

;

solve satisfy;
predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

constraint
   cp1d(variables, [2,2,4,5]) /\
   s = {2,3,4} /\
   n = 1 /\
   value = 2 /\

   open_atmost(s, n, variables, value)
;

output
[
  "variables: " ++ show(variables) ++ "\n" ++
  "value: " ++ show(value) ++ "\n" ++
  "n: " ++ show(n) ++ "\n" ++
  "s: " ++ show(s) ++ "\n"
];
