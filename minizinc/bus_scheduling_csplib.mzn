% 
% Bus driver scheduling problem (prob022 in CSPLib) in MiniZinc.
% 
% http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob022/index.html
% 
% From 
% http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob022/spec.html
% """
% Specification
% Bus driver scheduling can be formulated as a set paritioning problem. 
% We propose 12 set partitioning problems derived from small bus driver 
% scheduling problems. These consist of a given set of tasks (pieces of 
% work) to cover and a large set of possible shifts, where each shift 
% covers a subset of the tasks and has an associated cost. We must select 
% a subset of possible shifts that covers each piece of work once and 
% only once: this is called a partition. Further,
% 
% In the driver scheduling (unlike air crew scheduling) the main aim is 
% to reduce the number of shifts used in the solution partition and the 
% total cost of the partition is secondary. To simplify the problem we have 
% made the cost of each shift the same. This means that the goal is to 
% minimise the number of shifts.
% 
% The problems come from four different bus companies: 
% Reading (r1 to r5a), 
% CentreWest Ealing area (c1, c1a, c2), 
% the former London Transport (t1 and t2). 
%
% The problems have differing regulations and features (e.g. urban and 
% short distance rural bus schedules can have very different features). Note 
% that r1 and r1a are the same problem, but have different numbers of 
% generated shifts. Similarly with the problems: c1, c1a and r5, r5a. 
% 
% Problems are presented in the same format as the set partitioning 
% examples in ORLIB. The first line gives the number of rows (pieces of work), 
% columns (shifts) and the minimum number of columns need for a partition. 
% Then each line after that corresponds to one column. It starts with 
% the cost (which is always 1 in our case) then the number of rows it 
% covers, followed by the rows it covers. 
% """

%
% Note: This model skips the cost parameter.
%
% This is a MIP mode so the MIP solvers may also be used, e.g.
%  - MiniZinc's MIP solver
%  - ECLiPSe's eplex
% 
%
% Example, for the problem t1 
% (http://www.hakank.org/minizinc/bus_scheduling_csplib_t1.dzn)
% minizinc solver gives this solution:
% 
%  x: [0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1]
%
%  {11, 18, 19, 20}{1, 2, 14, 15}{3, 4, 7}{5, 6, 12, 13}{8, 9, 16, 17}{10, 22, 23}{0, 21}
% 

% Here are all data files:
%  http://www.hakank.org/minizinc/bus_scheduling_csplib_c1.dzn
%  http://www.hakank.org/minizinc/bus_scheduling_csplib_c1a.dzn
%  http://www.hakank.org/minizinc/bus_scheduling_csplib_c2.dzn
%  http://www.hakank.org/minizinc/bus_scheduling_csplib_r1.dzn
%  http://www.hakank.org/minizinc/bus_scheduling_csplib_r1a.dzn
%  http://www.hakank.org/minizinc/bus_scheduling_csplib_r2.dzn
%  http://www.hakank.org/minizinc/bus_scheduling_csplib_r3.dzn
%  http://www.hakank.org/minizinc/bus_scheduling_csplib_r4.dzn
%  http://www.hakank.org/minizinc/bus_scheduling_csplib_r5.dzn
%  http://www.hakank.org/minizinc/bus_scheduling_csplib_r5a.dzn
%  http://www.hakank.org/minizinc/bus_scheduling_csplib_t1.dzn
%  http://www.hakank.org/minizinc/bus_scheduling_csplib_t2.dzn
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: num_work;
int: num_shifts;
int: min_num_shifts;
array[1..num_shifts] of set of int: shifts;

array[1..num_shifts] of var 0..1: x;
var 0..num_shifts: tot_shifts;

% solve minimize tot_shifts;
solve :: int_search(
        x ++ [tot_shifts], 
        first_fail, 
        indomain_min, 
        complete) 
    minimize tot_shifts;
    % satisfy;

constraint
   tot_shifts = sum(x)
   /\
   forall(j in 0..num_work-1) (
       sum(i in 1..num_shifts) (x[i]*bool2int(j in shifts[i])) = 1
   )
   /\
   tot_shifts >= min_num_shifts

   % /\ % for solve satisfy (t1)
   % tot_shifts = 7
;


output [
  "tot_shifts: " ++ show(tot_shifts) ++ "\n" ++ 
  "x: " ++ show(x) ++ "\n"
] ++ 
[
  if fix(x[i]) = 1 then show(shifts[i]) else "" endif
  | i in 1..num_shifts
] ++ 
["\n"] ++
[
  if fix(x[i]) = 1 then show(i) ++ " " else "" endif
  | i in 1..num_shifts
] ++ ["\n"];