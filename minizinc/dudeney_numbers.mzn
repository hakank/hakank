% 
% Dudeney numbers in MiniZinc.
% 
% From Pierre Schaus blog post
% Dudeney number
%  http://cp-is-fun.blogspot.com/2010/09/test-python.html
% """
% I discovered yesterday Dudeney Numbers
% A Dudeney Numbers is a positive integer that is a perfect cube such that the sum 
% of its decimal digits is equal to the cube root of the number. There are only six 
% Dudeney Numbers and those are very easy to find with CP.
% I made my first experience with google cp solver so find these numbers (model below) 
% and must say that I found it very convenient to build CP models in python!
% When you take a close look at the line: 
%     solver.Add(sum([10**(n-i-1)*x[i] for i in range(n)]) == nb)
% It is difficult to argue that it is very far from dedicated 
% optimization languages!
% """
% 
% Also see: http://en.wikipedia.org/wiki/Dudeney_number
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 6;

% decision variables
array[1..n] of var 0..9: x;
var 1..pow(10,n): nb;
var 1..9*n+1: s;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint 
   nb = s*s*s
;

constraint   
   nb = sum(i in 1..n) (
        pow(10,(n-i))*x[i]
       )
;

constraint 
  s = sum(x)
;

output [
  "s: ", show(s), "\n",
  "nb: ", show(nb), "\n",
  "x: ", show(x), "\n",
] ++ ["\n"];