% 
% Prime looking problem in MiniZinc.
% 
% From Scampi's model PrimeLooking.scala
% """
% Martin Gardner Problem:
% Call a number "prime-looking" if it is composite but not divisible by 2,3 or 5.
% The three smallest prime-looking numbers are 49, 77 and 91. 
% There are 168 prime numbers less than 1000. 
% How many prime-looking numbers are there less than 1000?
%  
% @author Pierre Schaus pschaus@gmail.com
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 
int: n = 999;
var 2..n: x;
var 2..n: d1;
var 2..n: d2;

% solve satisfy;
% solve minimize x;
solve :: int_search([x,d1,d2], first_fail, indomain_min, complete) satisfy;

constraint
   x = d1*d2 /\ d1 <= d2
   /\
   forall(i in {2,3,5}) (
      x mod i > 0
   )
;

output [
  "x: " ++ show(x) ++ " (" ++ show(d1) ++ "*" ++ show(d2) ++ ")\n"
]
 ++ ["\n"]
;

