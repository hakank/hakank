% 
% product predicate in MiniZinc.
% 
% Note: The MiniZinc specification defines prod(), but it is not implemented
% in version <= 0.8.1. 
%
% The predicate prod() should be seen as a a very crude substitution.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

array[1..40] of var 1..1000: x;
var int: p;

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain_min", "complete") satisfy;


%
% product: p is result of multiplication of all the elements in R.
% Cf sum for addition.
% 
predicate prod(array[int] of var int: R, var int: p) = 
  let {
     int: t = length(R),
     array[1..t] of var int: a
  }
  in
  a[1] = R[1]
  /\ 
  forall(i in 2..t) ( 
      a[i] = a[i-1]*R[i]
  )
  /\
  p = a[t]
;


constraint
        % prod(x) = p % this don't work in version <= 0.8.1

        p > 0
        /\
        p = product(x) 
        /\
        increasing(x) % symmetry breaking

        % /\ p = 10
;

output [
  "x: ", show(x), "\n",
  "p: ", show(p), "\n",

];
