% 
% Global constraint all_different_consecutive_values in MiniZinc.
%
% From Global constraint catalogue:
% http://www.emn.fr/z-info/sdemasse/gccat/Calldifferent_consecutive_values.html
% """
% Purpose:
%   Enforce 
%    (1) all variables of the collection VARIABLES to take distinct values and 
%    (2) constraint the difference between the largest and the smallest values 
%        of the VARIABLES collection to be equal to the number of variables 
%        minus one (i.e., there is no holes at all within the used values).
%
% Example: (5,4,3,6)
% """ 

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 4;
int: m = 10;

array[1..n] of var 1..m: x;

solve satisfy;

predicate all_different_consecutive_values(array[int] of var int: variables) =

   alldifferent(variables) 
   /\
   max(variables) - min(variables) = length(variables) - 1
;

% copy 
predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
        x[i] = y[i]
    )
  )
; 


constraint
  % cp1d([5,4,3,6], x) /\
  all_different_consecutive_values(x)
;

output [ "x: " ++ show(x) ++ "\n" ];