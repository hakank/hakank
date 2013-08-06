% 
% Denny's Menu problem in MiniZinc.
%
% From http://mindyourdecisions.com/blog/2011/12/01/dennys-math-commercial
% """
% So there’s the question: how many different price combinations will 
% total $10 when menu items priced at $2, $4, $6, and $8?
% """
% 
% Answer: There are 6 different combinations:
%
% [5, 0, 0, 0]
% ----------
% [3, 1, 0, 0]
% ----------
% [1, 2, 0, 0]
% ----------
% [2, 0, 1, 0]
% ----------
% [0, 1, 1, 0]
% ----------
% [1, 0, 0, 1]


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 4;
array[1..n] of int: y = [2,4,6,8];

% decision variables
array[1..n] of var 0..10: x;



solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   sum(i in 1..n) ( x[i]*y[i]) = 10
;

output [
  show(x) ++ "\n"
] ++
[
  if fix(x[i] > 0) then
     show(x[i]) ++ " of " ++ show(y[i]) ++ "\n"
  else ""
  endif
  | i in 1..n  
] 
;

