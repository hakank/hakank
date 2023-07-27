% 
% Global constraint between_min_max in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cbetween_min_max.html
% """
% VAR is greater than or equal to at least one variable of the collection 
% VARIABLES and less than or equal to at least one variable of the 
% collection VARIABLES.
% 
% Example
%     (3, <1,1,4,8)
% 
% The between_min_max constraint holds since its first argument 3 is 
% greater than or equal to the minimum value of the values of the 
% collection <1,1, 4,8> and less than or equal to the maximum value of 
% <1,1,4,8>.
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 4;
array[1..n] of var 1..8: x;
var lb_array(x)..ub_array(x): nvar;

predicate between_min_max(var int: nvar, array[int] of var int: x) = 
  let {
    var lb_array(x)..ub_array(x): min_val,
    var lb_array(x)..ub_array(x): max_val
  }
  in
  minimum(min_val, x)
  /\
  maximum(max_val, x)
  /\
  nvar >= min_val
  /\
  nvar <= max_val

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
  cp1d([1,1,4,8],x)
  /\
  nvar = 3
  /\
  between_min_max(nvar, x)

;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "nvar: " ++ show(nvar) ++ "\n"
];
