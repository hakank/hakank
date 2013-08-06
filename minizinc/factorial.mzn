% 
% Factorial in MiniZinc.
% 
% Since the prod() don't work yet, this is an alternative
% version of calculating factorial.
%
% Max n:
%  flatzinc  :  9
%  fz        : 12
%  ic, eplex : 12
%  fd        : 10
%  tini      : 12
%              With -k 64 it >= 15, and presents the result in hex.
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 10;
var int: z;


predicate factorial(int: n, var int: res) =
   let {
      array[1..n] of var int: x
   }
   in
   x[1] = 1
   /\
   forall(i in 2..n) (
     x[i] = x[i-1]*i 
   )
   /\
   res = x[n] 

; 

solve satisfy;

constraint
   factorial(n, z)

;

output [
  "n: ", show(n),"\n",
  "z: ", show(z), "\n",
];

