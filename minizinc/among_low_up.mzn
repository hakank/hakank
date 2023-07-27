% 
% Global constraint among_low_up in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Camong_low_up.html
% """
% Between LOW and UP variables of the VARIABLES collection are assigned to 
% a value of the VALUES collection.

% Example
% (
% 1, 2,
% <9, 2, 4, 5>,
% <0, 2, 4, 6, 8>
% )
% 
% The among_low_up constraint holds since between 1 and 2 values 
% (i.e., in fact 2 values) of the collection of values <9, 2, 4, 5> 
% belong to the set of values {0, 2, 4, 6, 8}.
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

array[1..4] of var 0..9: x;
array[1..5] of var 0..9: y;
var int: low;
var int: up;


%
% among_low_up
% 
% low and up should probably be fixed before calling.
%
predicate among_low_up(var int: low, var int: up, array[int] of var int: x, array[int] of var int: y)=
  let {
    var int: nvar,
    int: n = card(index_set(x))
  }
  in
  nvar = sum(i in index_set(x)) (
     sum(j in index_set(y)) (
        bool2int(x[i] = y[j])
     )
  )
  /\
  nvar >= low 
  /\
  nvar <= up
  /\
  low >= 0
  /\
  up <= n

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
  cp1d([9,2,4,6],x)
  /\
  cp1d([0,2,4,6,8],y)
  /\
  among_low_up(low, up, x, y)
  /\
  low = 1
  /\
  up = 3
;


output
[ 
  "x:" ++ show(x) ++ "\n" ++ 
  "y:" ++ show(y) ++ "\n" ++ 
  "low:" ++ show(low) ++ "\n" ++ 
  "up:" ++ show(up) ++ "\n" 

];
