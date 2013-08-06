% 
% Project Euler problem 6 in MiniZinc.
% 
% http://projecteuler.net/index.php?section=problems&id=6
% """
% The sum of the squares of the first ten natural numbers is,
% 1^(2) + 2^(2) + ... + 10^(2) = 385
%
% The square of the sum of the first ten natural numbers is,
% (1 + 2 + ... + 10)^(2) = 55^(2) = 3025
% 
% Hence the difference between the sum of the squares of the first ten 
% natural numbers and the square of the sum is 3025 − 385 = 2640.
% 
% Find the difference between the sum of the squares of the first one 
% hundred natural numbers and the square of the sum.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 100;
var int: ss; % sum of the squares
var int: s; % the sum
var int: s2 = s*s; % square of the sum
var int: d = s2 - ss;

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
  ss = sum(i in 1..n) (i*i)
  /\
  s = sum(i in 1..n) (i)
;

output [
  "sum of the squares: ", show(ss),"\n",
  "square of the sum: ", show(s2), "\n",
  "square of the sum - sum of the squares = ", show(d), "\n"
]
;