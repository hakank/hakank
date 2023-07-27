% 
% Global constraint all_different except 0 in MiniZinc.
%
% From Global constraint catalogue:
% https://sofdem.github.io/gccat/gccat/Calldifferent_except_0.html
% """ 
% Enforce all variables of the collection VARIABLES to take distinct 
% values, except those variables that are assigned to 0.
% 
% Example
%    (<5, 0, 1, 9, 0, 3>)
% 
% The alldifferent_except_0 constraint holds since all the values 
% (that are different from 0) 5, 1, 9 and 3 are distinct.
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n;
array[1..n] of var 0..n: x;

solve satisfy;

predicate all_different_except_0(array[int] of var int: variables) =

   forall(i,j in index_set(variables) where i!= j) (
        (variables[i] > 0 /\ variables[j] > 0) -> variables[i] != variables[j]      
   )
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
  % cp1d([5,0,1,9,0,3], x) /\
  all_different_except_0(x)
  /\ increasing(x)

  % /\ 
  % count(x,0,2)
;

%
% data
%
n = 6;


output [ show(x) ++ "\n" ];
