% 
% 100 doors problem in MiniZinc.
%
% Problem from  
% http://rosettacode.org/wiki/100_doors
% """
% Problem: You have 100 doors in a row that are all initially closed. 
% You make 100 passes by the doors. The first time through, you visit every 
% door and toggle the door (if the door is closed, you open it; if it is open, 
% you close it). The second time you only visit every 2nd door 
% (door #2, #4, #6, ...). The third time, every 3rd door 
% (door #3, #6, #9, ...), etc, until you only visit the 100th door.
% 
% Question: What state are the doors in after the last pass? Which are open,
%  which are closed?
% 
% Alternate: As noted in this page's discussion page, the only doors that 
% remain open are whose numbers are perfect squares of integers. Opening 
% only those doors is an optimization that may also be expressed. 
% """
% 

% This is the unoptimized version. 
% Also see: hundred_doors_optimized.mzn
%           hundred_doors_optimized_array.mzn

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 100;
set of int: r = 1..n;

% use a 100 x 100 matrix...
array[r, r] of var 0..1: x; % 0: closed, 1: open
var set of r: s; % the result as set

solve :: int_search([x[i,j] | i,j in r], first_fail, indomain_min, complete) satisfy;

constraint

  % unoptimized version
   forall(j in r) (
       x[1,j] = 1
   )
   /\
   forall(pass in 2..n) (
      forall(j in r) (
         if j mod pass = 0 then
            (
              x[pass-1,j] = 1 <-> x[pass,j] = 0
            )
            /\
            (
              x[pass-1,j] = 0 <-> x[pass,j] = 1
            )
         else 
          x[pass,j] = x[pass-1, j]
         endif
      )
   )

   /\
   forall(j in r) (
      x[n, j] == 1 <-> j in s
   )

;

output [
    % show(x[n, j]) ++ " "
    if fix(x[n,j]) == 1 then show(j) else show(0) endif ++ " " 
    | j in r
]
++ 
[
 "\n" ++ show(s), "\n"
]
;
