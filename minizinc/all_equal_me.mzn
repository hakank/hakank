% 
% Global constraint allequal in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Call_equal.html
% """
% Constraint
%
%     all_equal​(VARIABLES)
%
% Purpose
%
%     Enforce all variables of the collection VARIABLES to take the same value.
%
% Example
%     ​(<5, 5, 5, 5>)
%
% The all_equal constraint holds since all its variables are fixed to value 5.
% 
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 4;
array[1..n] of var 0..6: x;

predicate all_equal_me(array[int] of var int: variables) = 
   let {
     int: low = min(index_set(variables)),
     int: up = max(index_set(variables))
   } in
   forall(i in low+1..up) (
     x[i] = x[i-1]
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] )
  )
; 


solve satisfy;

constraint
  % cp1d([5,5,5,5], x) % OK
  % cp1d([_,5,5,_], x) % OK
  % cp1d([5,5,5,6], x) % FAIL
  % /\
  all_equal_me(x)
;

output [
  show(x)
] ++ ["\n"]
