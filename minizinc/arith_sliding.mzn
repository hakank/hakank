% 
% Global constraint arith_sliding in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Carith_sliding.html
% """
%  Enforce for all sequences of variables var1, var2, ..., vari of the 
% VARIABLES collection to have (var1+var2+...+vari) RELOP VALUE.
% 
% Example
%  (
% <
%  var-0,
%  var-0,
%  var-1,
%  var-2,
%  var-0,
%  var-0,
%  var--3
%  >, <, 4
%  )
%
%  The arith_sliding constraint holds since all the following seven 
%  inequalities hold:
%
%         * 0 < 4,
%         * 0+0 <4,
%         * 0+0+1 <4,
%         * 0+0+1+2 <4,
%         * 0+0+1+2+0 <4,
%         * 0+0+1+2+0+0 <4,
%         * 0+0+1+2+0+0-3 <4.
%
%
% """ 

% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 7;
array[1..n] of var -3..3: x;
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


%
% Note: val should probably be a fixed value.
%
predicate arith_sliding(array[int] of var int: x, int: relop, var int: val) =
   let {
     int: lbx = min(index_set(x)),
     int: ubx = max(index_set(x)),
   }
   in
   forall(i in lbx..ubx) (
     let {
        var int: s = sum(j in lbx..i) (x[j])
     }
     in
     cmp(s, val, relop)
   )
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different size",
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
; 

solve satisfy;



constraint
   cp1d([0,0,1,2,0,0,-3],x)
   /\
   arith_sliding(x, -2, z)
   /\
   z = 4
;

output [
  "x: " ++ show(x) ++ "\n" ++ 
  "z: " ++ show(z)
];
