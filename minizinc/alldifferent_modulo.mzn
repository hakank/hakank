% 
% Global constraint alldifferent_modulo in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Calldifferent_modulo.html
% """
% Enforce all variables of the collection VARIABLES to have a distinct 
% rest when divided by M.
% 
% Example
% (<25, 1, 14, 3>, 5)
% 
% The equivalence classes associated with values 25, 1, 14 and 3 are 
% respectively equal to 
%    25 mod 5 = 0, 1 mod 5 = 1, 14 mod 5 = 4 and 3 mod 5 = 3. 
% Since they are distinct the alldifferent_modulo constraint holds.

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 4;
array[1..n] of var 1..25: x;
var 1..5: m;

predicate all_different_modulo(array[int] of var int: x, var int: m) =
  let {
     int: lbx = min(index_set(x)),
     int: ubx = max(index_set(x)),
     array[lbx..ubx] of var int: mods
  }
  in 
  forall(i in lbx..ubx) (
     mods[i] = x[i] mod m
  )
  /\
  all_different(mods)

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
  cp1d([25,1,14,3],x)
  /\
  m = 5
  /\
  all_different_modulo(x,m)
;


output
[
  "x: " ++ show(x) ++ "\n" ++
  "m: " ++ show(m) ++ "\n"
];
