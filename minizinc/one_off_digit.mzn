% 
% One off digit problem in MiniZinc.
% 
% From Richard Wiseman
% http://richardwiseman.wordpress.com/2012/10/19/its-the-friday-puzzle-182/
% """
% in the equation:
% 
%    43 + 57 = 207
% 
% every digit is precisely one way from its true value.  What is the correct sum?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

set of 0..9: D = 0..9;
int: n = 2+2+3;

array[1..n] of int: g = [4,3, 5,7, 2,0,7];

% decision variables
array[1..n] of var D: x;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

   x[1]*10+x[2] + x[3]*10+x[4] = x[5]*100+x[6]*10+x[7]
   /\
   % one off
   forall(i in 1..n) (
     abs(g[i]-x[i]) = 1
   )

;

output [
  show(x) ++ "\n" ++
  "sum: " ++ show([x[i] | i in 5..7])
];

