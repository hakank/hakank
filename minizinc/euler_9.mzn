% 
% Project Euler problem 9 in MiniZinc.
% 
% http://projecteuler.net/index.php?section=problems&id=9
% """
% A Pythagorean triplet is a set of three natural numbers, a < b < c, 
% for which, a^(2) + b^(2) = c^(2)
%
% For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).
%
% There exists exactly one Pythagorean triplet for which a + b + c = 1000.
% Find the product abc.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

var 0..500: a;
var 0..500: b;
var 0..500: c;

var int: prod = a*b*c;

solve satisfy;

constraint
   a < b /\ b < c /\
   a*a + b*b = c*c
   /\
   a + b + c = 1000

;

output [
  show([a,b,c]) ++ ": " ++ show(prod)

];