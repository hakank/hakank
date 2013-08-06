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

% This is the unoptimized version, alternative model.
% Also see: hundred_doors_unoptimized.mzn
%           hundred_doors_optimized.mzn
%           hundred_doors_optimized_array.mzn

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 100;
set of int: r = 1..n;

array[r] of var 0..1: x; % 0: closed, 1: open
var set of r: s; % the result as set

solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  x[1] = 1
  /\
  % unoptimized version, using temp array
   forall(j in 1..n) (
      let {
        array[r] of var 0..1: y
      }
      in
      y[1] = x[1]
      /\
      forall(pass in 2..n) (
         if j mod pass = 0 then
            (
              y[pass] = 1 <-> y[pass-1] = 0
            )
            /\
            (
              y[pass] = 0 <-> y[pass-1] = 1
            )
         else 
          y[pass] = y[pass-1]
         endif
      )
      /\
      x[j] = y[n]
   )

    /\
    forall(j in r) (
       x[j] == 1 <-> j in s
    )

;

output [
    if fix(x[j]) = 1 then show(j) else show(0) endif ++ " " 
    | j in r
]
++ 
[
 "\n" ++ show(s), "\n"
]
;
