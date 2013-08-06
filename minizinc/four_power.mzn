% 
% A simple mathematical problem in Minizinc
%
% From http://www.dailyindia.com/show/225105.php/Novel-way-to-generate-infinite-number-of-solutions-to-ancient-maths-puzzle
% 
% """
% "It's like a puzzle: can you find four fourth powers that add up to another fourth power?
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
set of 1..400: R = 1..400;

var R: x1;
var R: x2;
var R: x3;
var R: x4;
var R: x5;

array[1..5] of var R: a = [x1,x2,x3,x4,x5];

solve :: int_search(a, first_fail, indomain_min, complete) satisfy;
% solve satisfy;

constraint
  all_different(a) /\
  increasing(a)
  /\
  x1*x1*x1*x1 + x2*x2*x2*x2 + x3*x3*x3*x3 + x4*x4*x4*x4 = x5*x5*x5*x5
;

output [
  show(a) ++ "\n"
];

