% 
% Global constraint change in MiniZinc.
% 
% From Global Constraint Catalogue:
% https://sofdem.github.io/gccat/gccat/Cchange.html
% """
% NCHANGE is the number of times that constraint CTR holds on consecutive 
% variables of the collection VARIABLES.
% 
% Example
% (3, <4, 4, 3, 4, 1>, !=)
% (1, <1, 2, 4, 3, 7>, > )
% 
% In the first example the changes are located between values 4 and 3, 3 and 4, 
% 4 and 1.  Consequently, the corresponding change constraint holds since 
% its first argument NCHANGE is fixed to value 3.
% 
% In the second example the unique change occurs between values 4 and 3. 
% Consequently, the corresponding change constraint holds since its first 
% argument NCHANGE is fixed to 1.
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 


int: n = 5;
array[1..n] of var 1..4: x;
array[1..n] of var 1..7: y;
var 0..n: c_x;
var 0..n: c_y;

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

predicate change(var int: changes, array[int] of var int: x, int: opx) =
   let {
     int: lbx = min(index_set(x)),
     int: ubx = max(index_set(x))
   }
   in
   changes = sum(i in lbx+1..ubx) ( bool2int(cmp(x[i-1], x[i], opx)))
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

   cp1d([4, 4, 3, 4, 1], x)
   /\
   cp1d( [1, 2, 4, 3, 7], y)
   /\
   change(c_x, x, 10) % !=
   /\
   change(c_y, y, 2)  % >

;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "y: " ++ show(y) ++ "\n" ++
  "c_x: " ++ show(c_x) ++ "\n" ++
  "c_y: " ++ show(c_y) ++ "\n"
];
