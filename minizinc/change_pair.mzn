% 
% Global constraint change_pairs in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cchange_pair.html
% """
% NCHANGE is the number of times that the following disjunction holds:
%  (X1 CTRX X2) \/ (Y1 CTRY Y2) , where (X1, Y1) and (X2, Y2) correspond to 
% consecutive pairs of variables of the collection PAIRS.
% 
% Example
%  (
%     3, <
%     x-3 y-5,​
%     x-3 y-7,​
%     x-3 y-7,​
%     x-3 y-8,​
%     x-3 y-4,​
%     x-3 y-7,​
%     x-1 y-3,​
%     x-1 y-6,​
%     x-1 y-6,​
%     x-3 y-7
%     >,​!=,​>
%     )
%
% In the example we have the following 3 changes:
%  * One change between pairs x-3 y-8 and x-3 y-4 since 3 != 3 \/ 8 >​4,
%  * One change between pairs x-3 y-7 and x-1 y-3 since 3 != 1 \/ 7 > 3,
%  * One change between pairs x-1 y-6 and x-3 y-7 since 1 != 3 \/ 6 > 7.
%
% Consequently the change_pair constraint holds since its first 
% argument NCHANGE is assigned to 3.

% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 


int: n = 10;
array[1..n] of var 1..3: x;
array[1..n] of var 1..8: y;
var int: nchange;

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

%
% change_pairs(NCHANGE, X, Y, CTR1, CTR2)
%
predicate change_pairs(var int: nchange, array[int] of var int: x, array[int] of var int: y, int: ctr1, int: ctr2) =
    let {
       int: lbx = min(index_set(x)),
       int: ubx = max(index_set(x)),
    }
    in
    nchange = sum(i in lbx+1..ubx) (
     bool2int(
      cmp(x[i-1], x[i], ctr1) 
      \/
      cmp(y[i-1], y[i], ctr2)
     )
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

   % cp1d([3,3,3,3,3,3,1,1,1,3], x)
   % /\
   cp1d([5,7,7,8,4,7,3,6,6,7], y)
   /\ 
   change_pairs(nchange, x, y, 2, 2)
   % /\
   % nchange = 3
;

output
[
  "x: " ++ show(x) ++ "\n" ++ 
  "y: " ++ show(y) ++ "\n" ++ 
  "nchange: " ++ show(nchange) ++ "\n" 
];
