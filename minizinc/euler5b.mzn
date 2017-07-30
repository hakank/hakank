% 
% Euler #5 in MiniZinc.
%
% Problem 5
% """
% 2520 is the smallest number that can be divided by each of the numbers 
% from 1 to 10 without any remainder.
%
% What is the smallest number that is evenly divisible by all of the numbers 
% from 1 to 20?
% """

% Note: This model requires MiniZinc v 2 (functions).

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 20;
%                     232792560
array[1..n] of var 1..999999999: x;

%
% g = gcd(x,y)
% g is the gcd of x and y
%
function var int: gcd(var int: x, var int: y) =
  let {
    int: p = min(ub(x),ub(y));
    var 1..p: g;
    constraint
       exists(i in 1..p) (
          x mod i = 0 /\ y mod i = 0
          /\
          forall(j in i+1..p) (
             not(x mod j = 0 /\ y mod j = 0)
          )
          /\
          g = i
      );
  } in g
;

%
% cm = lcm(x,y)
% cm is the lcm of x and y
%
function var int: lcm(var int: x, var int: y) = x*y div gcd(x,y);

solve satisfy;

constraint
  x[1] = 1 /\
  forall(i in 2..n) (
     x[i] = lcm(i,x[i-1])
  )
;

output 
[
  show(x) ++ "\n" ++ 
  "x[n]: " ++ show(x[n]) ++ "\n",
];
