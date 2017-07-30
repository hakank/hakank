% 
% Job-shop problem in MiniZinc.
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: num_jobs;
int: num_machines;


array[1..num_jobs, 1..num_machines] of int: job_times;
array[1..num_jobs, 1..num_machines] of int: job_order;

% for cumulative
array[1..num_jobs] of int: one = [1 | i in 1..num_jobs];


% machines x jobs
% Start/End times for each machine and jobs
int: max_time;
array[1..num_jobs, 1..num_machines] of var 0..max_time: job_start;
array[1..num_jobs, 1..num_machines] of var 0..max_time: job_end;

var 0..max_time: earliest_end_time = max([job_end[machine,job] | 
                                              job in 1..num_jobs, machine in 1..num_machines]);

% t1 is before t2
predicate before(var int: t1, var int: t2) = t1 <= t2;

% t1 is after t2
predicate after(var int: t1, var int: t2) = t1 >= t2;


% solve satisfy;
solve :: int_search(
        % [job_start[machine,job] | machine in 1..num_jobs, job in 1..num_machines], 
        % [job_start[machine,job] | machine in 1..num_machines, job in 1..num_jobs],
        array1d(job_start),
        first_fail,
        indomain_split,
        complete
        )
     minimize earliest_end_time;
     % satisfy;


constraint
    % end times
    forall(job in 1..num_jobs, machine in 1..num_machines) (
        job_end[job,machine] = job_start[job,machine] + job_times[job,machine]
    ) % :: domain

    /\ % ensure non-overlaps of the jobs
    forall(machine in 1..num_machines) (
       cumulative([job_start[job,machine] | job in 1..num_jobs],
                  [job_times[job,machine] | job in 1..num_jobs],
                  one,
                  1) % :: domain
    )

    % /\ % testing: non-overlaps of the machines
    % forall(job in 1..num_jobs) (
    %    cumulative([job_start[job,machine] | machine in 1..num_machines],
    %               [job_times[job,machine] | machine in 1..num_machines],
    %               one,
    %               1)
    % )


   /\ % check the job order
   forall(job in 1..num_jobs) (
      forall(machine1,machine2 in 1..num_machines where machine1 < machine2) (
          if job_order[job,machine1] < job_order[job,machine2] then
             before(job_end[job,machine1], job_start[job,machine2])
          else 
             after(job_start[job,machine1], job_end[job,machine2])
          endif
      )
   ) % :: domain
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
  if machine = 1 then "\n" else " " endif ++
    show(job_times[job,machine])
  | job in 1..num_jobs, machine in 1..num_machines
]
++ 
["\n\nMachine order"] 
++
[  if machine = 1 then "\n" else " " endif ++
     show(job_order[job,machine])
   | job in 1..num_jobs, machine in 1..num_machines
]
++
[ "\njob_start:\n", show2d(job_start), "\n",
  "\njob_end:\n", show2d(job_end), "\n" 
] 
++
[ "\n\nSchedule\n"]
++
[
  if machine = 1 then "\n" ++ show(job) ++ " : "  else " " endif ++
       show(job_start[job,machine]) ++ ".." ++ 
       show(job_end[job,machine]) ++ " "
  | job in 1..num_jobs, machine in 1..num_machines
]
++
[ "\n\nEarliest end time: " ++ show(earliest_end_time) ++ "\n" ]
++
["\n"];