% Post office problem in Minizinc
%
% Problem statement:
% http://www-128.ibm.com/developerworks/linux/library/l-glpk2/
%
% From Winston "Operations Research: Applications and Algorithms":
% """
% A post office requires a different number of full-time employees working 
% on different days of the week [summarized below]. Union rules state that 
% each full-time employee must work for 5 consecutive days and then receive 
% two days off. For example, an employee who works on Monday to Friday 
% must be off on Saturday and Sunday. The post office wants to meet its 
% daily requirements using only full-time employees. Minimize the number 
% of employees that must be hired.
%
% To summarize the important information about the problem:
%
%   * Every full-time worker works for 5 consecutive days and takes 2 days off
%   * Day 1 (Monday): 17 workers needed
%   * Day 2 : 13 workers needed
%   * Day 3 : 15 workers needed
%   * Day 4 : 19 workers needed
%   * Day 5 : 14 workers needed
%   * Day 6 : 16 workers needed
%   * Day 7 (Sunday) : 11 workers needed
%
% The post office needs to minimize the number of employees it needs to hire to meet its demand. 
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

/*
int: Mon = 1;
int: Tue = 2;
int: Wed = 3;
int: Thu = 4;
int: Fri = 5;
int: Sat = 6;
int: Sun = 7;

% sets
set of 1..7: DAYS = 1..7; 
*/

enum DAYS = {Mon,Tue,Wed,Thu,Fri,Sat,Sun};

% requirements number workers per day
array[DAYS] of int: Need = [17, 13, 15, 19, 14, 16, 11];


% Decision variables. x[i]: No. of workers starting at day i
array[DAYS] of var 0..max(Need): x; 

% Objective: sum of total workers (started on a day)
var int: z = sum(i in DAYS) (x[i]);

solve :: int_search(x, first_fail, indomain_min, complete) minimize z;
% solve satisfy;

constraint
%  z <= 23 % for solve satisfy
%  /\
  forall(i in DAYS)  (
    x[i] >= 0
  )
  /\
  sum(i in DAYS where i != Tue  /\ i != Wed ) (x[i]) >= Need[ Mon ] /\
  sum(i in DAYS where i != Wed  /\ i != Thu ) (x[i]) >= Need[ Tue ] /\
  sum(i in DAYS where i != Thu  /\ i != Fri ) (x[i]) >= Need[ Wed ] /\
  sum(i in DAYS where i != Fri  /\ i != Sat ) (x[i]) >= Need[ Thu ] /\
  sum(i in DAYS where i != Sat  /\ i != Sun ) (x[i]) >= Need[ Fri ] /\
  sum(i in DAYS where i != Sun  /\ i != Mon ) (x[i]) >= Need[ Sat ] /\
  sum(i in DAYS where i != Mon  /\ i != Tue ) (x[i]) >= Need[ Sun ]
;

output 
[
  "z: " ++ show(z) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" 
  
];