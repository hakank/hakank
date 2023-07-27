% 
% Global constraint arith_or in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Carith_or.html
% """
% Enforce for all pairs of variables var1i,​var2i of the 
% VARIABLES1 and VARIABLES2 collections to have var1i 
% RELOP VALUE∨var2i RELOP VALUE.
% 
% Example
%     (
%     〈0,​1,​0,​0,​1〉,​
%     〈0,​0,​0,​1,​0〉,​=,​0
%     )
% 
% The constraint arith_or holds since, for all pairs of variables 
% var1i,​var2i of the VARIABLES1 and VARIABLES2 collections, there is 
% at least one variable that is equal to 0.

% """ 

% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 5;
array[1..n] of var 0..1: x;
array[1..n] of var 0..1: y;
var 0..9: z;

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


predicate arith_or(array[int] of var int: x, array[int] of var int: y, int: relop, var int: val) =

   forall(i in index_set(x)) (
     cmp(x[i], val, relop) \/  cmp(y[i], val, relop)
   )
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
; 

solve satisfy;

constraint
   cp1d([0,1,0,0,1],x)
   /\
   cp1d([0,0,0,1,0],y)
   /\
   arith_or(x, y, 0, z)
   /\
   z = 0
;



output
[
  "x: " ++ show(x) ++ "\n" ++
  "y: " ++ show(y) ++ "\n" ++ 
  "z: " ++ show(z)
];
