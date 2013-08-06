% 
% Project Euler problem 1 in MiniZinc.
% 
% http://projecteuler.net/index.php?section=problems&id=1
% """
% If we list all the natural numbers below 10 that are multiples of 3 or 5, 
% we get 3, 5, 6 and 9. The sum of these multiples is 23.
% 
% Find the sum of all the multiples of 3 or 5 below 1000.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 999; % _below_ 1000
array[1..n] of var 0..1: x;
var int: s = sum(i in 1..n) (x[i]*i);

solve satisfy;

constraint

  forall(i in 1..n) (
     if i mod 3 = 0 \/ i mod 5 = 0 then
        x[i] = 1
     else 
        x[i] = 0
     endif
  )

;

output [
  show(s)
];
