% 
% Bank card code in in MiniZinc.
% 
% From
% http://www.dcs.gla.ac.uk/~pat/cpM/JChoco/test/MathChallenge.java
% """
% My bank card has a 4 digit pin, abcd. I use the following facts to help me
% remember it:
%
% - no two digits are the same
% - the 2-digit number cd is 3 times the 2-digit number ab
% - the 2-digit number da is 2 times the 2-digit number bc
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 4;
var 0..9: a;
var 0..9: b;
var 0..9: c;
var 0..9: d;
array[1..n] of var 0..9: x = [a,b,c,d];



% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(x)
  /\
  (10*c+d) = 3*(10*a+b)
  /\
  (10*d+a) = 2*(10*b+c)
;

output [
 show(x), "\n",
];