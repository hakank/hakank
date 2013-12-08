% 
% Job-shop problem in MiniZinc.
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: num_machines;
int: num_jobs;


array[1..num_machines, 1..num_jobs] of int: job_times;
array[1..num_machines, 1..num_jobs] of int: job_order;

% for cumulative
array[1..num_machines] of int: one = [1 | i in 1..num_machines];


% machines x jobs
% Start/End times for each machine and jobs
int: max_time;
array[1..num_machines, 1..num_jobs] of var 0..max_time: job_start;
array[1..num_machines, 1..num_jobs] of var 0..max_time: job_end;

var 0..max_time: earliest_end_time = max([job_end[machine,job] | 
                                              machine in 1..num_machines, job in 1..num_jobs]);

% t1 is before t2
predicate before(var int: t1, var int: t2) = t1 <= t2;

% t1 is after t2
predicate after(var int: t1, var int: t2) = t1 >= t2;


% solve satisfy;
solve :: int_search(
        % [job_start[machine,job] | machine in 1..num_machines, job in 1..num_jobs], 
        [job_start[machine,job] | job in 1..num_jobs, machine in 1..num_machines], 
        first_fail, % smallest,
        indomain_min,
        complete
        )
     minimize earliest_end_time;
     % satisfy;


constraint
    % end times
    forall(machine in 1..num_machines, job in 1..num_jobs) (
        job_end[machine,job] = job_start[machine,job] + job_times[machine,job]
    ) :: domain

    /\ % ensure non-overlaps of the jobs
    forall(job in 1..num_jobs) (
       cumulative([job_start[machine,job] | machine in 1..num_machines],
                  [job_times[machine,job] | machine in 1..num_machines],
                  one,
                  1) :: domain
    )

    % /\ % testing
    % forall(machine in 1..num_machines) (
    %    cumulative([job_start[machine,job] | job in 1..num_jobs],
    %               [job_times[machine,job] | job in 1..num_jobs],
    %               one,
    %               1)
    % )


   /\ % check the job order
   forall(machine in 1..num_machines) (
      forall(job1,job2 in 1..num_jobs where job1 < job2) (
          if job_order[machine,job1] < job_order[machine,job2] then
             before(job_end[machine,job1], job_start[machine,job2])
          else 
             after(job_start[machine,job1], job_end[machine,job2])
          endif
      )
   ) :: domain
;


%
% Output
%
output 
[
  "\nEarliest end time: " ++ show(earliest_end_time) ++ "\n" ++
  "\nJob times:"
]
++
[ 
  if job = 1 then "\n" else " " endif ++
    show(job_times[machine,job])
  | machine in 1..num_machines, job in 1..num_jobs
]
++ 
["\n\nJob order"] 
++
[  if job = 1 then "\n" else " " endif ++
     show(job_order[machine, job])
   | machine in 1..num_machines, job in 1..num_jobs
]
++
[ "\n\nSchedule\n"]
++
[
  if job = 1 then "\n" ++ show(machine) ++ " : "  else " " endif ++
       show(job_start[machine,job]) ++ ".." ++ 
       show(job_end[machine,job]-1) ++ " "
  | machine in 1..num_machines,job in 1..num_jobs
]
++
[ "\n\nEarliest end time: " ++ show(earliest_end_time) ++ "\n" ]
++
["\n"];