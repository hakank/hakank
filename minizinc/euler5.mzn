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

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 20;
%                     232792560
array[1..n] of var 1..999999999: x;

%
% gcd(x,y,g)
% g is the gcd of x and y
%
predicate gcd(var int: x, var int: y, var int: g) =
  let {
    int: p = min(ub(x),ub(y))
  } in
   exists(i in 1..p) (
       x mod i = 0 /\ y mod i = 0
       /\
       forall(j in i+1..p) (
           not(x mod j = 0 /\ y mod j = 0)
       )
       /\
       g = i
   )
;

%
% lcm(x,y,cm)
% cm is the lcm of x and y
%
predicate lcm(var int: x, var int: y, var int: cm) =
    let {
       int: p = min(ub(x),ub(y)),
       var 1..p: g
    } in
    gcd(x,y,g)
    /\
    cm = x*y div g
;

solve satisfy;

constraint
  x[1] = 1 /\
  forall(i in 2..n) (
     lcm(i,x[i-1],x[i])
  )
;

output 
[
  show(x) ++ "\n" ++ 
  "x[n]: " ++ show(x[n])
];
