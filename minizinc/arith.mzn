% 
% Global constraint arith in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Carith.html
% """
% Enforce for all variables var of the VARIABLES collection to have 
%    var RELOP VALUE.
% 
% Example
% (<4, 5, 7, 4, 5>, <, 9)
% 
% The arith constraint holds since all values of the collection 
% <4, 5, 7, 4, 5> are strictly less than 9.
% """ 

% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 5;
array[1..n] of var 1..7: x;
var 0..9: y;

solve satisfy;

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


predicate arith(array[int] of var int: x, int: relop, var int: val) =

   forall(i in index_set(x)) (
     cmp(x[i], val, relop)
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


constraint
   cp1d([4,5,7,4,5],x)
   /\
   arith(x, -2, y)
   /\
   y = 9
;


output
[
  "x: " ++ show(x) ++ "\n" ++
  "y: " ++ show(y) ++ "\n"
];
