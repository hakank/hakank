% 
% my_pow in MiniZinc.
%
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

var int: n;
var int: num;


%
% Since the builtin pow() don't handle (var int, int)
% I'm rolling my own.
% Note: This don't handle (var int, var int), just (var int, int)
%
predicate my_pow(var int: n, int: len, var int: n_pow) =
   let {
      array[1..len+1] of var int: x
   }
   in
   x[1] = 1
   /\
   forall(i in 2..len+1) (
     x[i] = x[i-1] * n
   )
   /\
   n_pow = x[len+1]
;

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
  n = 3
  /\
  my_pow(n, 4, num)
;

output
[
  "n:" ++ show(n) ++ "\n" ++
  "num:" ++ show(num) ++ "\n" 
];