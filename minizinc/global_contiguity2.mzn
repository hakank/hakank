% 
% Global constraint global_contiguity in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cglobal_contiguity.html
% """
% Enforce all variables of the VARIABLES collection to be assigned to 0 or 1. 
% In addition, all variables assigned to value 1 appear contiguously.
% 
% Example:
% (<0,​1,​1,​0>)
% 
% The global_contiguity constraint holds since the sequence 0 1 1 0 contains 
% no more than one group of contiguous 1.
% """

% Another take of this. Compare with
%  http://www.hakank.org/minizinc/global_contiguity.mzn

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 10;
array[1..n] of var 0..1: x;

%
% Decomposition of global_contiguity
%
predicate global_contiguity(array[int] of var 0..1: x) =
  let { 
    int: len = length(x),
    array[1..len] of var 0..2: y
  }
  in
  increasing(y) /\
  forall(i in index_set(x)) (
     x[i] = 1 <-> y[i] = 1
  )
  /\
  (sum(x) = 0 <-> sum(y) = 0)

;

solve satisfy;

constraint
  global_contiguity(x)
;

output [ 
  "x: " ++ show(x) ++ "\n"
];
