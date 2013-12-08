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


%
% The timeline, for simplifying the presentation: 
% Does machine m does something at time t?
%
% array[0..max_time, 1..num_machines] of var 0..1: timeline;

% t1 is before t2
predicate before(var int: t1, var int: t2) = t1 <= t2;

% t1 is after t2
predicate after(var int: t1, var int: t2) = t1 >= t2;

% solve satisfy;
% solve minimize earliest_end_time;
solve :: int_search(
        [job_start[machine,job] | job in 1..num_jobs, machine in 1..num_machines]
        ,
        first_fail,
        indomain_split,
        complete
        )
     minimize earliest_end_time;
     % satisfy;


constraint
    % end times
    forall(machine in 1..num_machines, job in 1..num_jobs) (
        job_end[machine,job] = job_start[machine,job] + job_times[machine,job]
    ) 

    /\ % ensure non-overlaps of the jobs
    forall(job in 1..num_jobs) (
       cumulative([job_start[machine,job] | machine in 1..num_machines],
                  [job_times[machine,job] | machine in 1..num_machines],
                  one,
                  1) % :: domain
    )

    % /\ % testing (if we don't require ordering of jobs)
    % forall(machine in 1..num_machines) (
    %    cumulative([job_start[machine,job] | job in 1..num_jobs],
    %               [job_times[machine,job] | job in 1..num_jobs],
    %               one,
    %               1) :: domain
    % ) 


   /\ % ensure that the jobs are in order
   forall(machine in 1..num_machines) (
      forall(job1,job2 in 1..num_jobs where job1 < job2) (
          if job_order[machine,job1] < job_order[machine,job2] then
             before(job_end[machine,job1], job_start[machine,job2]) % :: domain
          else 
             after(job_start[machine,job1], job_end[machine,job2]) % :: domain
          endif
      )
   ) 

   % /\ % For simplifying the presentation:
   %    % does machine m do anything at time t?
   % forall(t in 0..max_time, machine in 1..num_machines) ( 
   %       exists(job in 1..num_jobs) (           
   %          t >= job_start[machine,job] 
   %          /\
   %          t <  job_end[machine,job]
   %       )  <-> timeline[t,machine] = 1
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
[ "\n\nMachines:\n"]
++
[
  if job = 1 then "\n" ++ show(machine) ++ " : "  else " " endif ++
     show(job_start[machine,job]) ++ ".." ++ 
     show(job_end[machine,job]-1) ++ " "
  | machine in 1..num_machines,job in 1..num_jobs
]
++
[ "\n\nJobs:\n"]
++
[
  if machine = 1 then "\n" ++ show(job) ++ " : "  else " " endif ++
     show(job_start[machine,job]) ++ ".." ++ 
     show(job_end[machine,job]-1) ++ " "
  | job in 1..num_jobs, machine in 1..num_machines
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
%     if machine = num_machines /\ job = num_jobs then "|" else "" endif

%   else 
%      ""
%   endif
    
%   | time in 0..max_time, machine in 1..num_machines, job in 1..num_jobs
% ]
++
[ "\n\nMachine times:\n" ]
++
[  
   "machine " ++ show(machine) ++ ": " ++ 
    show(fix(min([job_start[machine, job] | job in 1..num_jobs])))  ++ ".." ++
    show(fix(max([job_end[machine, job] | job in 1..num_jobs])))  ++ "\n"
   | machine in 1..num_machines
]
++
[ "\nJob times:\n" ]
++
[  
   "job " ++ show(job) ++ ": " ++ 
    show(fix(min([job_start[machine, job] | machine in 1..num_machines])))  ++ ".." ++
    show(fix(max([job_end[machine, job] | machine in 1..num_machines])))  ++ "\n"
   | job in 1..num_jobs
]
++
["\nEarliest end time: " ++ show(earliest_end_time) ++ "\n"]
++
["\n"];