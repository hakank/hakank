% 
%  Recreational mathematics in MiniZinc.
% 
%  From Pascal Van Henrentyck "The OPL Optimization Programming Language", 
%  page 32:
%  """
%  Consider finding an eight digit number that is a square and remains a square
%  when 1 is concatenated in front of its decimal notation.
%  """
%
%  Compare with the Comet model: 
%  http://www.hakank.org/comet/number_square.co
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

var 10000000..99999999: n;
var 0..20000: x;
var 0..20000: y;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  n == x*x /\
  100000000+n ==y*y 
;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "y: " ++ show(y) ++ "\n"
];