% 
% Global constraint among_modulo in MiniZinc.
% 
% From Global Constraint Catalogue
% http://www.emn.fr/x-info/sdemasse/gccat/Camong_modulo.html
% """
% NVAR is the number of variables of the collection VARIABLES taking a value 
% that is congruent to REMAINDER modulo QUOTIENT.
% 
% Example
% (3, <4, 5, 8, 4, 1>, 0, 2)
% 
% In this example REMAINDER = 0 and QUOTIENT = 2 specifies that we count the 
% number of even values taken by the different variables. As a consequence 
% the among_modulo constraint holds since exactly 3 values of the 
% collection <4, 5, 8, 4, 1> are even.

% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

array[1..5] of var 1..8: x;
var 0..100: nvar;
var 0..100: remainder;
var 0..100: quotient;


%
% among_modulo
% 
predicate among_modulo(var int: nvar, array[int] of var int: x, var int: remainder, var int: quotient)=
  nvar = sum(i in index_set(x)) (
    bool2int(x[i] mod quotient = remainder)
  )
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
  cp1d([4,5,8,4,1],x)
  /\
  among_modulo(nvar, x, remainder, quotient)
  /\
  remainder = 0
  /\
  quotient = 2
  % /\
  % nvar = 3
;


output
[
  "x: " ++ show(x) ++ "\n" ++
  "nvar: " ++ show(nvar) ++ "\n" ++
  "remainder: " ++ show(remainder) ++ "\n" ++
  "quotient: " ++ show(quotient) ++ "\n"
];