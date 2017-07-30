% 
% Job-shop problem in MiniZinc.
% 
% This version prints more information about the solution.
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: num_machines;
int: num_jobs;


array[1..num_jobs, 1..num_machines] of int: job_times;
array[1..num_jobs, 1..num_machines] of int: job_order;

% for cumulative
array[1..num_jobs] of int: one = [1 | i in 1..num_jobs];

% If the first order index starts with 1 then we have to compensate below
int: order_add = if min([job_order[1,machine] | machine in 1..num_machines]) = 1 then 1 else 0 endif;

% machines x jobs
% Start/End times for each machine and jobs
int: max_time;
array[1..num_jobs, 1..num_machines] of var 0..max_time: job_start;
array[1..num_jobs, 1..num_machines] of var 0..max_time: job_end;

var 0..max_time: earliest_end_time = max([job_end[job,machine] | 
                                              job in 1..num_jobs, machine in 1..num_machines]);

%
% The timeline, for simplifying the presentation: 
% Does machine m does something at time t?
%
% array[0..max_time, 1..num_jobs] of var 0..1: timeline;

% t1 is before t2
predicate before(var int: t1, var int: t2) = t1 <= t2;

% t1 is after t2
predicate after(var int: t1, var int: t2) = t1 >= t2;


% solve satisfy;
% solve minimize earliest_end_time;
% ann: var_select;
% ann: val_select;
solve :: int_search(
        [job_end[job,machine] | machine in 1..num_machines, job in 1..num_jobs] 
        % ++
        % [job_start[job,machine] | machine in 1..num_machines, job in 1..num_jobs] 
        ,
        smallest, % first_fail,
        indomain_min, % indomain_split,
        complete
        )
     minimize earliest_end_time;
     % satisfy;

constraint
    % end times
    forall(job in 1..num_jobs, machine in 1..num_machines) (
        job_end[job,machine] = job_start[job,machine] + job_times[job,machine]
    ) 

    /\ % ensure non-overlaps of the jobs
    forall(machine in 1..num_machines) (
       cumulative([job_start[job,machine] | job in 1..num_jobs],
                  [job_times[job,machine] | job in 1..num_jobs],
                  one,
                  1) % :: domain
    )

    % /\ % testing (if we don't require ordering of jobs)
    % forall(job in 1..num_jobs) (
    %    cumulative([job_start[job,machine] | machine in 1..num_machines],
    %               [job_times[job,machine] | machine in 1..num_machines],
    %               one,
    %               1) :: domain
    % ) 


   /\ % ensure that the jobs are in order
   forall(job in 1..num_jobs) (
      forall(machine1,machine2 in 1..num_machines where machine1 < machine2) (
          if job_order[job,machine1] < job_order[job,machine2] then
             before(job_end[job,machine1], job_start[job,machine2]) % :: domain
          else 
             after(job_start[job,machine1], job_end[job,machine2]) % :: domain
          endif
      )
   ) 

   /\ % Experiment
   forall(job in 1..num_jobs) ( 

     % redundant constraints 
     all_different([job_start[job,machine] | machine in 1..num_machines]) % :: domain
     /\
     all_different([job_end[job,machine] | machine in 1..num_machines])  % :: domain
    

     /\ % extra constraint to ensure the order of jobs for each machine
     forall(machine in 1..num_machines) (
       % also compensate if we have job_order indexed by 1
       job_order[job,machine] = sum([bool2int(job_start[job,machine] > job_start[job,machine2]) | machine2 in 1..num_machines]) + order_add
       /\
       job_order[job,machine] = sum([bool2int(job_end[job,machine] > job_end[job,machine2]) | machine2 in 1..num_machines]) + order_add
     )

   )


   % /\ % For simplifying the presentation:
   %    % does machine m do anything at time t?
   % forall(t in 0..max_time, job in 1..num_jobs) ( 
   %       exists(machine in 1..num_machines) (           
   %          t >= job_start[job,machine] 
   %          /\
   %          t <  job_end[job,machine]
   %       )  <-> timeline[machine,t] = 1
   % )
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
["\n\nJob order"] 
++
[  if machine = 1 then "\n" else " " endif ++
     show(job_order[job,machine])
   | job in 1..num_jobs, machine in 1..num_machines
]
++
[ "\n\nMachines per job:\n"]
++
[
  if machine = 1 then "\n" ++ show(job) ++ " : "  else " " endif ++
     show(job_start[job,machine]) ++ ".." ++ 
     show(job_end[job,machine]-1) ++ " "
  | job in 1..num_jobs,machine in 1..num_machines
]
++
[ "\n\nJobs per machine:\n"]
++
[
  if job = 1 then "\n" ++ show(machine) ++ " : "  else " " endif ++
     show(job_start[job,machine]) ++ ".." ++ 
     show(job_end[job,machine]-1) ++ " "
  | machine in 1..num_machines, job in 1..num_jobs
]
% ++ ["\n\nSchedule:"] ++
% [
%   if time < fix(earliest_end_time) then 
%      if job = 1 /\ machine = 1 then 
%          "\nTime: " ++ show(time) ++ ": " ++
%          if time < 10 then " " else "" endif ++
%          if time < 100 then " " else "" endif ++
%          if time < 1000 then " " else "" endif
%      else "" 
%      endif ++ 

%     %  if time >= fix(job_start[machine,job]) /\ time < fix(job_end[machine,job]) then
%     %      "|m" ++ show(machine) ++ ": j" ++ show(job) ++ "  " ++
%     %      if job < 10 then " " else "" endif
%     %  else 
%     %      if job = 1 /\ fix(timeline[time,machine]) = 0 then
%     %          "|m" ++ show(machine) ++ ": -   "  ++
%     %          if job < 10 then " " else "" endif
%     %      else 
%     %         "" 
%     %      endif
%     % endif ++
 
%     % end the line with "|"
%     if machine = num_jobs /\ job = num_machines then "|" else "" endif

%   else 
%      ""
%   endif
    
%   | time in 0..max_time, machine in 1..num_jobs, job in 1..num_machines
% ]
++
[ "\n\nJob times:\n" ]
++
[  
   "job " ++ show(job) ++ ": " ++ 
    show(fix(min([job_start[job,machine] | machine in 1..num_machines])))  ++ ".." ++
    show(fix(max([job_end[job,machine] | machine in 1..num_machines])))  ++ "\n"
   | job in 1..num_jobs
]
++
[ "\nMachine times:\n" ]
++
[  
   "machine " ++ show(machine) ++ ": " ++ 
    show(fix(min([job_start[job,machine] | job in 1..num_jobs])))  ++ ".." ++
    show(fix(max([job_end[job,machine] | job in 1..num_jobs])))  ++ "\n"
   | machine in 1..num_machines
]
++
["\nEarliest end time: " ++ show(earliest_end_time) ++ "\n"]
++
["\n"];