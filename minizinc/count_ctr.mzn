% 
% Global constraint count (with relops) in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Ccount_ctr.html
% """
% Let N be the number of variables of the VARIABLES collection assigned to 
% value VALUE; Enforce condition N RELOP NVAR to hold.
% 
% Example
%   (5, <4, 5, 5, 4,5>, >=,2)
% 
% The count constraint holds since value VALUE=5 occurs 3 times within 
% the items of the collection VARIABLES=<4, 5, 5, 4, 5>, which is greater 
% than or equal to (RELOP is set to >=) NVAR=2.
% """
% 
% Note: The constraint count in globals.mzn does not have the relop constraint.
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 5;
array[1..n] of var 1..5: x;
var lb_array(x)..ub_array(x): nvar;
var 0..n: val;


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
% count_ctr
% The global constraint catalogue calls this predicate count, but there are
% already a count in globals.mzn, without the relop stuff.
%
predicate count_ctr(var int: val, array[int] of var int: x, int: relop, var int: nvar) =
    sum(i in index_set(x)) (
       bool2int(cmp(x[i], val, relop))
    ) >= nvar
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 

constraint
   cp1d([4,5,5,4,5], x)
   /\
   val = 5
   % /\
   % nvar = 2
   /\
   count_ctr(val, x, 1, nvar)

;


output [
 "x: " ++ show(x) ++ "\n" ++ 
 "nvar: " ++ show(nvar) ++ "\n" ++
 "val: " ++ show(val) ++ "\n"
];
