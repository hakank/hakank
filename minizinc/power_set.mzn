%
% Power set in Minizinc.
%
% Creates a power set of 1..n.
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

%
% p should be pow(2,n) but the integer version of pow don't work in 
% MiniZinc version 0.7.1 .
%
predicate powerset(int: n, array[int] of var set of int: x) =
  let {
     int: p = pow(2, n), % p = 2^n
     array[1..p] of var set of 1..n: z 
  }
  in
  all_different(z) /\
  cp1d(x, z)
;
predicate cp1d(array[int] of var set of int: x, array[int] of var set of int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

int: n = 3;
int: size = pow(2, n); % 2^n;
array[1..size] of var set of 1..size: a;

% This gives a power set of 1..n
constraint 
    powerset(n, a)
    % symmetry breaking: order the sets
    /\ increasing(a) % don't work for all solvers
;

output [
  show(a), "\n"

];
