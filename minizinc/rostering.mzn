% 
% Rostering problem in Minizinc.
% 
% Assign persons to different tasks.
% 
% Problem inspired by
% Jean-Charles Regin "Modeling Problems in Constraint Programming"
% ( http://www.constraint-programming.com/people/regin/papers/modelincp.pdf  )
% page 19ff
% 
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 


% week schedule for green (1), red (2), blue (3), yellow (4) 
int: mon = 1;
int: sun = 7;
int: num_shifts = 3;
int: num_workers = 4;
array[mon..sun,1..num_shifts] of var 1..num_workers: on_duty; 
array[1..num_workers] of var 0..7: num_workdays;
array[1..num_shifts] of int: workload = [4,2,1];

% objective
var int: z = sum(i in 1..7, j in 1..num_shifts) (on_duty[i,j]);

% solve satisfy;
% solve :: int_search([on_duty[i,j] | i in mon..sun, j in 1..num_shifts] , first_fail, indomain_min, complete)  minimize z;
solve :: int_search([on_duty[i,j] | i in mon..sun, j in 1..num_shifts] , first_fail, indomain_min, complete)  satisfy;

% Make the schedule as fair as possible, i.e. approx. the same 
% number of days per week.
constraint
  % at most one shift a day
  forall(i in mon..sun) (
       forall(j,k in 1..num_shifts where j < k) (
             on_duty[i,j] != on_duty[i,k]
       )
  ) /\

  % all must work at least x shift per week
  forall(w in 1..num_workers) (
      num_workdays[w] = sum(j in mon..sun, k in 1..num_shifts) ( bool2int(on_duty[j,k] == w)) 
      % approx. equal number of days
      /\ num_workdays[w] >= ( (num_shifts*7) div num_workers )

       % different shift has different work load
      /\ sum(j in mon..sun, k in 1..num_shifts) ( workload[k]*bool2int(on_duty[j,k] == w))  <= 13
  )

;

output [
   "on_duty: ", show(on_duty), "\n",
   "num_workdays: ", show(num_workdays), "\n",
   "z: ", show(z), "\n"
]

