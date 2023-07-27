% 
% Global constraint product_ctr in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cproduct_ctr.html
% """
% Constraint
%
%     product_ctr​(VARIABLES,​CTR,​VAR)​
%
% Purpose
%
%     Constraint the product of a set of domain variables. More precisely, 
%     let P denote the product of the variables of the VARIABLES collection. 
%     Enforce the following constraint to hold: P CTR VAR.
%
% Example
%     (<2,1,4>,=,8)
%
%     The product_ctr constraint holds since its last argument VAR=8 is 
%     equal (i.e., CTR is set to =) to 2*1*4.
%
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 3;
array[1..n] of var 1..4: variables;
int: ctr = 0;
var 1..1000: t_var;

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

predicate product_ctr(array[int] of var int: variables,
                      int: ctr,
                      var int: t_var) =
       let {
          var int: res
       } in
       res = product(variables) /\
       cmp(res,t_var, ctr)
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
   cp1d(variables, [2,1,4]) /\
   t_var = 8 /\

   product_ctr(variables, ctr, t_var)

;

output [
"variables: " ++ show(variables) ++ "\n"++
"ctr: " ++ show(ctr) ++ "\n"++
"t_var: " ++ show(t_var) ++ "\n"
];
