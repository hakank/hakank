%
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
%   * Every full-time worker works for <days_to_work> consecutive days 
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
% This is a a more general model than post_office_problem.mzn
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "nosets.mzn";

int: n = 7;
set of 0..6: DAYS = 0..6; % monday 0 
array[DAYS] of int: need; % need for each day

% decision variables
array[DAYS] of var 0..100: x; % number of workers that started that day
array[DAYS] of var 0..100: x_worked; % number of workers that worked that day
array[DAYS] of var 0..100: excess; % number of extra workers per day

var 1..3: days_to_work; % the number of days each person works
var 0..100: tot_days = sum(i in DAYS) (x[i]);  % total number of work days
var 0..100: num_workers = sum(x); % total number of workers

% objective function
var 0..100: z = tot_days;


% solve :: int_search(x, first_fail, indomain_split, complete) minimize z;
solve minimize z;

constraint
  forall(day in DAYS) (
     x_worked[day] = sum([x[(n+day-k) mod n] | k in 0..days_to_work-1]) /\
     x_worked[day] >= need[day] /\
     excess[day] = x_worked[day] - need[day]
  )
;

%
% data
%
need = array1d(DAYS,[17, 13, 15, 19, 14, 16, 11]);

output 
[
  "x       : \(x)\n" ++
  "x_worked: \(x_worked)\n" ++
  "excess  : \(excess)\n" ++    
  "z: \(z)\n" ++
  "num_workers: \(num_workers)\n",
  "tot_days: \(tot_days)\n",  
  "days_to_work: \(days_to_work)\n",
  "\n",
]++
[
 "day:\(day): work days:\([(n+day-k) mod n | k in 0..fix(days_to_work)-1]) sum = \(sum([fix(x[(n+day-k) mod n]) | k in 0..fix(days_to_work)-1]))\n"
 | day in DAYS
];
