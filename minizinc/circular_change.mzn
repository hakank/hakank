% 
% Global constraint circular change in MiniZinc.
% 
% From Global Constraint Catalogue:
% https://sofdem.github.io/gccat/gccat/Ccircular_change.html
% """
% NCHANGE is the number of times that  CTR  holds on consecutive variables of 
% the collection VARIABLES. The last and the first variables of the 
% collection VARIABLES are also considered to be consecutive.
% 
% Example
% (4, <4, 4, 3, 4, 1>, !=)
% 
% In the example the changes within the VARIABLES = <4, 4, 3, 4, 1> collection 
% are located between values 4 and 3, 3 and 4, 4 and 1, and 1 and 4 (i.e., 
% since the third argument CTR of the circular_change constraint is set to 
% !=, we count one change for each disequality constraint between two 
% consecutive variables that holds). Consequently, the corresponding 
% circular_change constraint holds since its first argument NCHANGE is 
% fixed to 4.

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 


int: n = 5;
array[1..n] of var 1..4: x;
var 0..n*n: c;

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
predicate cmp(var int: a, var int: b, -12..12: t) = 
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

predicate circular_change(var int: changes, array[int] of var int: x, int: xop) =
   let {
     int: lbx = min(index_set(x)),
     int: ubx = max(index_set(x)),
   }
   in
   changes = bool2int(cmp(x[n],x[1], xop)) +
             sum(i in lbx+1..ubx) ( bool2int(cmp(x[i-1], x[i], xop)))
;

solve satisfy;



constraint
   
  % x = [4,4,3,4,1]
  %/\
  circular_change(c, x, 10)
  /\
  c = 4

;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "c: " ++ show(c) ++ "\n"
];
