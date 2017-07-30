% 
% Scheduling with multiple workers in MiniZinc.
% 
% Constraint Programming: Scheduling with multiple workers
% http://stackoverflow.com/questions/34477453/constraint-programming-scheduling-with-multiple-workers
% """
% I'm new to constraint programming. I imagine this is an easy problem but I can't 
% wrap my head around it. Here's the problem:
% 
%  - We have multiple machines (N), each with a limited resource (let's say 
%    memory, and it can be the same for all machines.)
%  - We have T tasks, each with a duration and each requiring some amount of the 
%    resource.
%  - A machine can work on multiple tasks at the same time as long as its 
%    resource isn't exceeded.
%  - A task cannot be split among machines and it has to be done in one shot 
% (i.e. no pausing).
% 
% How do we assign the tasks to the machines to minimize the end-time or the 
% number of machines used?
% 
% It seems like I should be able to achieve this with the cumulative predicate but 
% it seems like it's limited to scheduling one set of tasks to one worker with a 
% limited global resource rather than a variable number of workers.
% 
% I'm just learning CP & MiniZinc. Any ideas on how to generalize cumulative? 
% Alternatively, is there an existing MiniZinc model I can understand that does 
% something like this (or close enough?)
% 
% Thanks,
% 
% PS: I don't have any concrete data since this is a hypothetical/learning exercise for 
% the most part. Imagine you have 10 machines and 10 tasks with various durations 
% (in hours): 2,4,6,5,2,1,4,6,3,2,12 with memory requirements (GBs): 
% 1,2,4,2,1,8,12,4,1,10. Each machine has 32 GBs of ram.
% """
%
% Note: There are 11 durations, so I skip the penultimate element (2).
% 
% Also, this model is a little more general since it allows to state the RAM for each machine.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_tasks = 10; 
int: num_machines = 10;

array[1..num_tasks] of int: duration = [2,4,6,5,2,1,4,6,3,12]; % duration of tasks, original
% array[1..num_tasks] of int: duration   = [1,2,3,4,5,6,7,8,9,10]; % duration of tasks
% array[1..num_tasks] of int: duration   = [2,4,6,5,2,1,4,6,3,2]; % duration of tasks
% array[1..num_tasks] of int: duration = [12,13,14,15,16,17,18,19,20,21]; % duration of tasks

array[1..num_tasks] of int: memory   = [1,2,4,2,1,8,12,4,1,10];  % RAM requirements (GB), original
% array[1..num_tasks] of int: memory     = [1,2,3,4,5,6,7,8,9,10]; % RAM requirements (GB)
% array[1..num_tasks] of int: memory   = [11,12,13,14,15,16,17,18,19,20];    % RAM requirements (GB)

int: max_time = 30; 

% RAM for each machine (GB)
% array[1..num_machines] of int: machines_memory = [32 | i in 1..num_machines]; % original, 32GB for each machine
array[1..num_machines] of int: machines_memory = [32,32,32,32,32,32,32,32,32,32];
% array[1..num_machines] of int: machines_memory = [64,64,64,64,32,32,32,32,32,32];


% 
% decision variables
%
array[1..num_tasks] of var 1..max_time: start_time; % start time for each task
array[1..num_tasks] of var 1..max_time: end_time;   % end time for each task
array[1..num_tasks] of var 1..num_machines: machine; % which machine to use
array[1..num_machines,1..max_time] of var 0..max(machines_memory): machine_used_ram;

var 1..num_machines: machines_used = max(machine);
var 1..max_time: last_time = max(end_time);

% solve satisfy;
% solve minimize machines_used;
% solve minimize last_time;
solve :: int_search(start_time ++ machine ++ array1d(machine_used_ram), first_fail, indomain_split, complete)  minimize last_time;
% solve :: int_search(start_time ++ machine ++ array1d(machine_used_ram), first_fail, indomain_split, complete) minimize machines_used;


constraint
  forall(t in 1..num_tasks) (
    end_time[t] = start_time[t] + duration[t] -1
  ) 
  % /\ cumulative(start_time,duration,[1 | i in 1..num_tasks],machines_used)
  /\
  forall(m in 1..num_machines) (
     % check the memory of the times when a task is run on the machine
     forall(tt in 1..max_time) (
        machine_used_ram[m,tt] = sum([memory[t]*(machine[t]=m)*(tt in start_time[t]..end_time[t]) | t in 1..num_tasks]) /\ 
        machine_used_ram[m,tt] <= machines_memory[m]

        % sum([memory[t]*(machine[t]=m)*(tt in start_time[t]..end_time[t]) | t in 1..num_tasks]) <= machines_memory[m]
     )
     % /\ % machines start at time 1
     % (sum([machine_used_ram[m,tt] | tt in 1..max_time]) > 0) -> machine_used_ram[m,1] >= 1
  )

  %  ensure that machine m is used before machine m+1 (for machine_used)
  /\ value_precede_chain([i | i in 1..num_machines],machine)
;

output [
  "start_time: \(start_time)\n",
  "durations : \(duration)\n",
  "end_time  : \(end_time)\n",
  "memory    : \(memory)\n",
  "last_time : \(last_time)\n",
  "machine   : \(machine)\n",
  "machines_used: \(machines_used)\n",
  % "machine_used_ram: \(machine_used_ram)\n",
]
++
[ "Machine memory per time:\n    "]
++
[ show_int(3,tt) | tt in 1..max_time ]
++
[
 if tt = 1 then "\n" ++ "M" ++ show_int(2, m) ++ ": "  else " " endif ++
   show_int(2,machine_used_ram[m,tt])
 | m in 1..num_machines, tt in 1..max_time
]
++ ["\n\nTime / task: machine(task's memory)\n  Task "] ++
[
  show_int(7,t)
  | t in 1..num_tasks
]
++ 
[
  if t = 1 then "\nTime " ++ show_int(2,tt) ++ " " else " " endif ++
    if tt in fix(start_time[t])..fix(end_time[t]) then
      show_int(2,fix(machine[t])) ++ "(" ++ show_int(2,memory[t]) ++ ")"
    else 
      "      " 
    endif 
  | tt in 1..fix(last_time), t in 1..num_tasks
] 
;

