% 
% Global constraint range_ctr in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Crange_ctr.html
% """
% Constraint
%
%    range_ctr​(VARIABLES,​CTR,​VAR)​
%
% Purpose
%
%    Constraint the difference between the maximum value and the minimum 
%    value of a set of domain variables. More precisely, let R denote 
%    the difference between the largest and the smallest variables of 
%    the VARIABLES collection. Enforce the following constraint to 
%    hold: R CTR VAR.
%
% Example
%   (<1,9,4>,=,8)
%
%    The range_ctr constraint holds since its last argument VAR=8 
%    is equal (i.e., CTR is set to =) to max(1,9,4)-min(1,9,4).
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 3;
array[1..3] of var 1..9: variables;
int: ctr = 0;
var 1..ub_array(variables): t_var; 


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
predicate cmp(var int: a, var int: b, int: t) = 
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


predicate range_ctr(array[int] of var int: variables, int: ctr, var int: t_var) =
   let {
       var int: mmin = min(variables),
       var int: mmax = max(variables),
       var int: res = mmax - mmin
   } in
   cmp(t_var, res, ctr)
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
  cp1d(variables, [1,9,4]) /\
  t_var = 8 /\

  range_ctr(variables, -1, t_var)
  
;

output [
"variables: " ++ show(variables) ++ "\n"++
"t_var: " ++ show(t_var) ++ "\n"
];
