% 
% Digital roots in MiniZinc.
% 
% 
% http://en.wikipedia.org/wiki/Digital_root
% """
% The digital root (also repeated digital sum) of a number is the number 
% obtained by adding all the digits, then adding the digits of that number, 
% and then continuing until a single-digit number is reached.
% 
% For example, the digital root of 65,536 is 7, because 6 + 5 + 5 + 3 + 6 = 25 
% and 2 + 5 = 7.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 5;
int: m = 112;
int: x = 65536;
var int: d_x;
array[1..m] of var int: d_roots;

%
% digital root 
%
predicate digital_root(var int: num, var int: res, var int: base) =
   res = 1 + ((num-1) mod (base-1))
;


% solve satisfy;
solve :: int_search(d_roots, first_fail, indomain_min, complete) satisfy;

constraint
   forall(i in 1..m) (
      digital_root(i, d_roots[i],10)
   )
   /\
   digital_root(x, d_x, 10)
;

output 
[ 
  "x: ", show(x), "\n",
  "d_x: ", show(d_x), "\n",
] ++
[
  % "digital_roots: ", show(d_roots), "\n"
  show(i) ++ ":" ++ show(d_roots[i]) ++ "\n"
  | i in 1..m
] ++ ["\n"];