% 
% Global constraint among_diff_0 in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Camong_diff_0.html
% """
% NVAR is the number of variables of the collection VARIABLES that take a 
% value different from 0.
% 
% Example
% (3, <0, 5, 5, 0, 1>)
% 
% The among_diff_0 constraint holds since exactly 3 values of the collection 
% of values <0, 5, 5, 0, 1> are different from 0.
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 5;
array[1..n] of var 0..5: x;
var int: nvar;

predicate among_diff_0(var int: nvar, array[int] of var int: x)=

  nvar = sum(i in index_set(x)) (
    bool2int(x[i] != 0)
  )
;

% generalized version:
%   number of variables that are different from a value c
predicate among_diff_c(var int: nvar, array[int] of var int: x, var int: c)=
  nvar = sum(i in index_set(x)) (
    bool2int(x[i] != c)
  )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
;


solve satisfy;

constraint
  % cp1d([0,5,5,0,1],x)
  % /\
  % among_diff_0(nvar, x)
  among_diff_c(nvar, x, 0)
;


output [
   "nvar: " ++ show(nvar) ++ "\n" ++
   "x:" ++ show(x)

];

