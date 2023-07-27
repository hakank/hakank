% 
% Global constraint discrepancy in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cdiscrepancy.html
% """
% discrepancy(VARIABLES,​K)
% 
% Purpose
%
% K is the number of variables of the collection VARIABLES that take their 
% value in their respective sets of bad values.
%
% Example
%     (
%     <
%     var-4  bad-{1, 4, 6},
%     var-5  bad-{0, 1},
%     var-5  bad-{1, 6, 9},
%     var-4  bad-{1,​4},
%     var-1  bad-{}
%     >, 2
%     )
%
% The discrepancy constraint holds since exactly K=2 variables 
% (i.e., the first and fourth variables) of the VARIABLES collection 
% take their value within their respective sets of bad values.
%
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 5;
array[1..n] of var 1..5: variables;
array[1..n] of var set of 0..9: bad;
var int: k;

predicate discrepancy(array[int] of var int: v, array[int] of var set of int: bad, var int: k) =
    k = sum(i in index_set(variables)) (
      bool2int(
         variables[i] in bad[i]
      )
    )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 

predicate cp1d(array[int] of var set of int: x, array[int] of var set of int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint 
   cp1d(bad,
         [
            {1,4,6},
            {0,1},
            {1,6,9},
            {1,4},
            {}
         ])
    /\
    cp1d(variables,[4,5,5,4,1])
    /\
    k = 2
   /\
   discrepancy(variables, bad, k)
;

output
[
  "variables: " ++ show(variables) ++ "\n" ++
  "bad: " ++ show(bad) ++ "\n" ++
  "k: " ++ show(k)
];
