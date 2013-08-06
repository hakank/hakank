% 
% Work-shift scheduling problem in MiniZinc.
% 
% From SAS/OR 9.22 User's Guide: Constraint Programming
% """
% This example illustrates the use of the GCC constraint in finding a 
% feasible solution to a work-shift scheduling problem and then using 
% the element constraint to incorporate cost information in order to 
% find a minimum cost schedule.
% 
% Six workers (Alan, Bob, John, Mike, Scott, and Ted) are to be assigned 
% to three working shifts. The first shift needs at least one and at 
% most four people; the second shift needs at least two and at most 
% three people; and the third shift needs exactly two people. Alan 
% does not work on the first shift; Bob works only on the third shift. 
% The others can work any shift. The objective is to find a feasible 
% assignment for this problem.
% 
% You can model the minimum and maximum shift requirements with a 
% GCC constraint and formulate the problem as a standard CSP. The 
% variables W1–W6 identify the shift to be assigned to each of the 
% six workers: Alan, Bob, John, Mike, Scott, and Ted. 
% """

% Note: There are 34 solutions. The solution in the above example
% is:
%    x: [2, 3, 1, 1, 2, 3]
% 
%    Shift 1: John Mike 
%    Shift 2: Alan Scott 
%    Shift 3: Bob Ted 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: s = 3; % 3 working shifts
int: p = 6; % 6 workers

array[1..s] of int: lower = [1,2,2];
array[1..s] of int: upper = [4,3,2];
array[1..p] of string: workers = ["Alan", "Bob", "John", "Mike", "Scott", "Ted"];
        
% Six workers (Alan, Bob, John, Mike, Scott and Ted)
array[1..p] of var 1..s: x;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  global_cardinality_low_up(x, [i | i in 1..s], lower, upper)
  /\
  x[1] != 1 % Alan doesn't work on the first shift.
  /\
  x[2] = 3 % Bob works only on the third shift.
;

output [
  "x: " ++ show(x)
]
++
[
  if worker = 1 then "\nShift " ++ show(shift) ++ ": " else "" endif ++
    if (fix(x[worker]) = shift) then 
       show(workers[worker]) ++ " "
    else "" endif
  | shift in 1..s, worker in 1..p
]
 ++ ["\n"]
;

