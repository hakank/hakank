% 
% Number of runs in a sequence in MiniZinc.
% 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 6;
array[1..n] of var 1..n: x;
var int: r;

solve satisfy;

%
% runs
%
% (The number of runs is the number of switches + 1.)
%
predicate runs(var int: nruns, array[int] of var int: x) =
  let {
     int: n = length(x),
     array[1..n] of var 0..1: runs_ix
  }
  in
  forall(i in 2..n) (
    runs_ix[i] = 1 <-> x[i-1] = x[i]
  )
  /\
  runs_ix[1] = 1
  /\
  nruns = 1 + sum(i in 1..n) ( bool2int(runs_ix[i] = 0))
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

constraint
  cp1d(x, [1,1,2,2,1,1])
  /\
  runs(r, x)
%  /\
%  r = 3

;

output [
"x: " ++ show(x) ++ "\n"++
"r: " ++ show(r) ++ "\n"
];
