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

% This is the optimized version, using and array.
% Also, see: hundred_doors_unoptimized.mzn 
%            hundred_doors_optimized.mzn (using set)

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 100;
set of int: r = 1..n;

array[r] of var 0..1: x;

solve satisfy;

constraint

    % optimized version, array version
    forall(j in r) (
      (
       % is j a square?
       exists(i in r) (
        i*i = j
       ) <-> x[j] = 1
      )
    )


;


output [
   "x: ", show(x), "\n",
];