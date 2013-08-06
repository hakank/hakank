% 
% PuzzlOr problem in MiniZinc.
% 
% From PuzzlOr Feb, 2012
% http://www.informs.org/ORMS-Today/Private-Articles/February-Volume-39-Number-1/THE-PUZZLOR
% """
% This is no way to run a Popsicle stand
% By John Toczek
% 
% Workforce management is central to efficient operations and good customer service. 
% Proper scheduling of employees can mean the difference between profitability and 
% business failure.
% 
% As the manager of a Popsicle stand, you are required to hire and set the weekly 
% work schedule for your employees. The required levels for the week are as follows:
% Total employees required: 
%
%    Monday = 5, 
%    Tuesday = 7, 
%    Wednesday = 7, 
%    Thursday = 10, 
%    Friday = 16, 
%    Saturday = 18; 
%    Sunday = 12. 
%
% Assume the same staffing requirements continue week after week.
%
% Full-time employees work five consecutive days and earn $100 per day. 
% Part-time employees work two consecutive days and earn $150 per day.
%
% Question:
% 
% What is the minimal weekly staffing cost you can achieve 
% while meeting the required staffing levels?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 7;
% number of required employees per per day
array[0..n-1] of int: demand = array1d(0..n-1, [5,7,7,10,16,18,12]);

array[0..n-1] of var 0..120: full_time;
array[0..n-1] of var 0..120: part_time;

array[0..n-1] of var 0..30: all_f;
array[0..n-1] of var 0..30: all_p;
array[0..n-1] of var 0..130: all;

% cost
% full time, $100 per day, work for 5 days
% part time, $150 per day, work 2 days
var 0..10000: z = sum(i in 0..n-1) ( 5*full_time[i]*100 + 2*part_time[i]*150 );

% solve minimize z;
solve :: int_search(
         full_time ++ part_time, 
         input_order, 
         indomain_min, 
         complete) 
     % minimize z;
     satisfy;

constraint
    forall(i in 0..n-1) (
      % trace(show([abs((7+j) mod 7) | j in i-4..i] ) ++ "   " ++
      %       show([abs((7+j) mod 7) | j in i-1..i]) ++ "\n", 1=1)
      % /\
      let {
         array[0..4] of var int: f = array1d(0..4, [full_time[abs((7+j) mod 7)] | j in i-4..i]),
         array[0..1] of var int: p = array1d(0..1, [part_time[abs((7+j) mod 7)] | j in i-1..i])
      } in
       all_f[i] = sum(f) 
       /\
       all_p[i] = sum(p)
       /\
       all[i] = sum(f) + sum(p)
       /\ 
       all[i] >= demand[i]
    )
;

% Expanded version
% constraint
%    all_f[0] = sum([full_time[i] | i in [0,3,4,5,6]]) /\
%    all_f[1] = sum([full_time[i] | i in [1,4,5,6,0]]) /\
%    all_f[2] = sum([full_time[i] | i in [2,5,6,0,1]]) /\
%    all_f[3] = sum([full_time[i] | i in [3,6,0,1,2]]) /\
%    all_f[4] = sum([full_time[i] | i in [4,0,1,2,3]]) /\
%    all_f[5] = sum([full_time[i] | i in [5,1,2,3,4]]) /\
%    all_f[6] = sum([full_time[i] | i in [6,2,3,4,5]]) /\
%
%    all_p[0] = sum([part_time[i] | i in [0,6]]) /\
%    all_p[1] = sum([part_time[i] | i in [1,0]]) /\
%    all_p[2] = sum([part_time[i] | i in [2,1]]) /\
%    all_p[3] = sum([part_time[i] | i in [3,2]]) /\
%    all_p[4] = sum([part_time[i] | i in [4,3]]) /\
%    all_p[5] = sum([part_time[i] | i in [5,4]]) /\
%    all_p[6] = sum([part_time[i] | i in [6,5]]) /\
%
%    forall(i in 0..n-1) (
%       all[i] = all_f[i] + all_p[i] /\
%       all[i] >= demand[i]
%    )
% ;

% For solve satisfy
constraint z <= 8200;


output [
  "z        : " ++ show(z)         ++ "\n" ++
  "full_time: " ++ show(full_time) ++ "\n" ++
  "part_time: " ++ show(part_time) ++ "\n" ++
  "all_f    : " ++ show(all_f)     ++ "\n" ++ 
  "all_p    : " ++ show(all_p)     ++ "\n" ++ 
  "all      : " ++ show(all)       ++ "\n" ++
  "demand   : " ++ show(demand)    ++ "\n" 
]
 ++ ["\n"]
;

