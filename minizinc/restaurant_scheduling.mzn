% 
% Restaurant Scheduling problem (PuzzlOR) in MiniZinc.
% 
% From PuzzlOR
% Restaurant Scheduling (April 2008)
% http://puzzlor.editme.com/Scheduling
% """
% April 2008 - Restaurant Scheduling
% 
% After faithfully serving the O.R. profession for 50 years, you decide 
% to retire and open a restaurant. Among the hundreds of details with 
% opening a restaurant, you need to hire and schedule employees. Based 
% on the foot traffic of other restaurants in the area, you expect that 
% you will need the following number of employees each day:
% 
% Employees Schedule 	 
% Day of week 	Employees Needed
% Monday 	4
% Tuesday 	5
% Wednesday 	5
% Thursday 	10
% Friday 	12
% Saturday 	12
% Sunday 	2
% 
% Your employees will work four consecutive days and then have three 
% days off. They will be paid $100 for each day they work.  In your 
% rush to get the restaurant started, you haphazardly hire 17 employees. 
% Five will start on Monday, five will start on Thursday and seven will 
% start on Friday. This schedule satisfies the above work requirements, 
% but you have no idea how optimal this is.
% 
% Questions:
% 1. How much money would you save each week from your current schedule 
%    if you optimized your workforce?
% 2. How much additional money would you save or lose each week if 
%    you switched your employees to a "five days on, two days off"
%    schedule at $80 per day?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: days = 7;

int: work_days = 4;
% int: work_days = 5;
int: free_days = days-work_days;
int: cost_per_day = 100;
% int: cost_per_day = 80;

% Needed per day
%                                M T W  T  F  S S
array[1..days] of int: needed = [4,5,5,10,12,12,2];

% decision variables
% Number to hire each day
array[1..days] of var 0..20: x;
 % number of people working this day
array[1..days] of var 0..20: working;

var int: total_cost = sum(i in 1..days) ( work_days*cost_per_day*x[i]);

% solve satisfy;
% solve minimize total_cost;
solve :: int_search(x, first_fail, indomain_min, complete) minimize total_cost;


constraint
   %x[1] = 5 /\ x[4] = 5 /\ x[5] = 7 /\ sum([x[i] | i in 1..days]) = 17
   %/\
   % forall(d in 1..days) (
   %    working[d] = sum(w in d..d+work_days-1) (
   %                   x[w]  
   %                 ) 
   %    /\ working[d] >= needed[d]
   % )
   % /\
   % forall(d in 1..work_days) (
   %      x[d] = x[d+days]
   % )
   % /\
   forall(d in 1..days) (
     let {
        array[1..work_days] of var int: tmp= [x[i] | i in d..min(days,d+work_days-1)] ++ [x[i] | i in 1..d-work_days where d + work_days -1 > days]
     } in
     trace("d: " ++ show(tmp) ++ "\n", 1=1) /\
     working[d] = sum(tmp)
     /\
     working[d] >= needed[d]
   )
;


output [ 
  "needed     : " ++ show(needed) ++ "\n" ++
  "total_cost : " ++ show(total_cost) ++ "\n" ++
  "x          : " ++ show(x) ++ "\n" ++
  "working    : " ++ show(working) ++ "\n" ++
  "num_workers: " ++ show(sum([x[i] | i in 1..days]))
] 
 ++ ["\n"]
;

