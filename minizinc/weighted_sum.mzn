% 
% Global constraint weighted_sum in MiniZinc.
% 
% Note: This constraint is not in the Global Constraint Catalogue.
% 
% The predicate weighted_sum will calculate the weighted sum of
% two arrays
%   coeffs * x
% where the sum is constrained with a limit and an comparision operator
% (coded as -3..3)
% 
% E.g.
% coeffs = [1, 2, -3] and x = [4, 4, 0]
% will give the weighted sum of 12
% 


%   

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 3;

array[1..n] of var -3..2: coeffs;
array[1..n] of var 0..5: x;
var int: s;

%
% Since MiniZinc don't handle function variables we use the following
% hack where t is the type of comparison operator.
% t:
%  - 2 :  a <  b
%  - 1 :  a <= b
%    0 :  a  = b
%    1 :  a >= b
%    2 :  a >  b
% else : a != b
%
predicate cmp(var int: a, var int: b, -2..2: t) =
   if     t = -2 then
     a < b
   elseif t = -1 then
     a <= b
   elseif t =  0 then
     a = b
   elseif t =  1 then
     a >= b
   elseif t =  2 then
     a > b
   else
     a != b
   endif
;


%
% weighted_sum(sum, coeffs, x, limit, operation)
%
predicate weigthed_sum(var int: s, array[int] of var int: coeffs, array[int] of var int: x, var int: limit, int: xop) =
  s = sum(i in index_set(x)) (
     coeffs[i]*x[i]
  )
  /\
  cmp(s, limit, xop)
;

%
%
%
predicate weigthed_sum(var int: s, array[int] of var int: coeffs, array[int] of var int: x) =
  s = sum(i in index_set(x)) (
     coeffs[i]*x[i]
  )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

% solve satisfy;
solve :: int_search(x ++ coeffs, first_fail, indomain_min, complete) satisfy;

constraint
   cp1d(coeffs, [1,2,-3])
   /\
   % weigthed_sum(coeffs, x, 12, 2)
   weigthed_sum(s, coeffs, x)
;


output [
"s: " ++ show(s) ++ "\n"++
"coeffs: " ++ show(coeffs) ++ "\n"
];
