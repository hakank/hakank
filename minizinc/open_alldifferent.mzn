% 
% Global constraint open_alldifferent in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Copen_alldifferent.html
% """
% open_alldifferent(S, VARIABLES)
% 
% Purpose
%
% Let V be the variables of the collection VARIABLES for which the 
% corresponding position belongs to the set S. Enforce all variables of 
% V to take distinct values.
%
% Example
%  ({2, 3, 4}, <9,1,9,3>)
%
% The open_alldifferent constraint holds since the last three (i.e., 
% S = {2, 3, 4}) values of the collection 9,1,9,3 are distinct.
% 
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 4;
array[1..n] of var 1..9: x;
var set of 1..n: s;

predicate open_alldifferent(array[int] of var int: x, var set of int: ss) =
    forall(i, j in index_set(x) where i < j) (
        (i in ss /\ j in ss) -> x[i] != x[j]      
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint

   cp1d(x, [9,1,9,3])
   % /\
   % s = {2,3,4}

   /\
   open_alldifferent(x, s)
;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "s: " ++ show(s) ++ "\n"
];
