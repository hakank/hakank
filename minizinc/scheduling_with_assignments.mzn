%
% Scheduling with assignments of workers in Minizinc.
%
% The reason I wrote this model was that most (if not all) 
% examples of scheduling (using cumulative) didn't show the 
% assignments of the individual workers.
% 
% This MiniZinc model features the following:
%
%   - the order of jobs is calculated using cumulative.
%
%   - it shows the assignments of workers to the job in several ways:
%     * assignment matrices for both jobs to workers and workers to jobs
%     * timelines for jobs as well as for workers (Gantt like)
%     (Note: the timeline for the jobs is hard to get aligned in a pretty
%      way.)
%
%   - can handle precedences between jobs (when do_precedence = true).
%     There are some examples of this (via the link below).
%
%   - can enforce that idleness is not allowed (with allow_idle=false), 
%     i.e. that a worker cannot be idle until his/her last task.
%     This is for testing the Rule 1 from Ron L. Graham's paper 
%      "Combinatorial Scheduling Theory". See below for more comments.
%     This is tested in the models 
%        scheduling_with_assignments10*.dzn
%
%     Note: Enforcing non-idleness can make the solving time much 
%     longer, and can lead to some unexpected results (which is discussed
%     in Graham's paper.)
%
%   - can enforce that the workers assigned to a tasks must be "near" i.e.
%     as a collected block. This is set by collect_workers = true.
%
%     This might lead to nicer (Gantt-like) output and solve some more 
%     type of problems, e.g. perfect square placement problems.
%     It can also make the result sub-optimal compared with a version
%     without this enforcement.
%
%     It should be considered experimental.
%     
%
% Some problem instances has been collected here:
% http://www.hakank.org/minizinc/#scheduling_with_assignments
%


%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: num_jobs; % number of things
int: num_workers; % number of worker
int: max_time;

array[1..num_jobs] of 0..max_time: duration;
array[1..num_jobs] of 0..num_workers: resource;

%
% precedences
%
bool: do_precendences; % if we should care about precedences
% number of precedences. 
% Note: Must be >= 1 since MiniZinc don't accept 0 size arrays/matrices
int: num_precedences;
% Note: If there are no precedences then a
% dummy precedence must be used, e.g. 0,0
array[1..num_precedences, 1..2] of int: precedences;

%
% Allow idle workers?
%
% If false then enforce that there can be no idleness until last task 
% (for this worker) 
% Note: This is Rule 1 in Ron L. Graham "Combinatorial Scheduling Theory":
% "No assembler can be idle if there is some job he or she can be
%  doing."
% See Graham's paper how the greedyness of this rule causes some "paradoxes" 
% in scheduling.
% 
% Also see especially scheduling_with_assignments10*.dzn where this is 
% tested in some details.
% 
bool: allow_idle;

% 
% Collect workers
% 
% If true then the tasks must be done using "near" workers,
% i.e. as a block. 
% This might lead to some nicer (Gantt-like) output and also 
% solve some more problems, e.g. perfect square placements.
%
bool: collect_workers;


% decision variables
% Start/End times for the jobs
array[1..num_jobs] of var 0..max_time: start_time;
array[1..num_jobs] of var 0..max_time: end_time = 
                          [start_time[job] + duration[job] | job in 1..num_jobs];

% Assignments of workers for the jobs (0/1 matrix)
array[1..num_jobs,1..num_workers] of var 0..1: assignment;

% Timespan (to minimize)
var 0..max_time: earliest_end_time = max([ end_time[j] | j in 1..num_jobs]);


% solve satisfy;
% solve minimize earliest_end_time; 
% ann: var_select;
% ann: val_select;
% solve :: int_search(
%         start_time,
%         smallest, % first_fail,
%         indomain_split, % indomain_min, % indomain_split,
%         complete)
%     minimize earliest_end_time; 
%     % satisfy;

solve :: seq_search(
       [int_search(start_time, first_fail, indomain_split, complete),
        % Note: SICStus want this extra for the "fancy" output
        % int_search([assignment[i,j] | i in 1..num_jobs, j in 1..num_workers],
        %        input_order, indomain_min, complete)
       ]
       )
      minimize earliest_end_time;


%
% Check/ensure that there are no overlaps between two jobs.
%
predicate no_overlap(var int:s1, int:d1, var int:s2, int:d2) =
    s1 + d1 <= s2 \/ s2 + d2 <= s1
;



%
% Precedences: start[job1] must be done before start[job2]
%
predicate prec(int:job1 , int: job2, array[int] of var int: start, array[int] of var int: dur) =
   start[job1] + dur[job1] <= start[job2]
;


% %
% % See http://www.hakank.org/minizinc/disjunctive.mzn
% %
% predicate disjunctive(array[int] of var int: v_origin, 
%                       array[int] of var int: v_duration, 
%                       array[int] of var int: v_end_time, 
%                       array[int] of var int: v_used 
%                       ) =
%   assert(
%         length(v_origin) == length(v_duration) /\
%         length(v_duration) == length(v_end_time) /\
%         length(v_end_time) == length(v_used),
%         "All arrays must be of same length!")
%   /\
%   forall(i in index_set(v_origin)) (
%      ( 
%        v_duration[i] > 0 <-> (
%           v_used[i] = 1 
%           /\
%           v_end_time[i] = v_origin[i] + v_duration[i]
%           /\
%           forall(j in index_set(v_origin) where i < j) (
%             v_end_time[i] <= v_origin[j] % i left of j
%             \/
%             v_origin[i] >= v_end_time[j] % i right of j
%           )     
%        )
%      )
%      /\ % zero duration -> not used
%      ( 
%       v_duration[i] = 0 <-> (v_used[i] = 0 /\ v_end_time[i] = 0)
%      )        
%   )
% ;

% predicate disjunctive2(array[int] of var int: start, array[int] of var int: dur) =
%       assert(index_set(start) == index_set(dur), "disjunctive: " ++
%            "first and second arguments must have the same index set",
%         cumulative(start, dur, [ 1 | i in index_set(start) ], 1)) :: domain
% ;


constraint

   % get the job order
   cumulative(start_time, duration, resource, num_workers) % :: domain

   /\ % assignments
   forall(job in 1..num_jobs) (

       % Check number of workers working with the job
       resource[job] = sum(w in 1..num_workers) (
           bool2int(assignment[job,w] = 1)
       )

       % If two jobs overlaps then no worker can be assigned 
       % to both.
       % /\ 
       % forall(j2 in 1..num_jobs where job < j2) (
       %     not(no_overlap(start_time[job], duration[job],
       %                    start_time[j2], duration[j2]))
       %     ->
       %     forall(w in 1..num_workers) (
       %         assignment[job,w]+assignment[j2,w] <= 1
       %     )
       % )

       /\ % Alternative version which seems to be little faster:
          % If a person is assigned to two jobs, then these jobs 
          % cannot overlap.
       forall(j2 in 1..num_jobs where job < j2) (
          exists(w in 1..num_workers) (
             assignment[job,w] + assignment[j2,w] = 2
          )
          -> 
          no_overlap(start_time[job], duration[job],
                     start_time[j2], duration[j2])
       )


       /\ % If the resources of two jobs > total number of workers
          % they cannot overlap. 
       forall(j2 in 1..num_jobs where job < j2) (
           if resource[job] + resource[j2] > num_workers then
               no_overlap(start_time[job], duration[job],
                          start_time[j2], duration[j2])
           else 
              true
           endif
       )

   )


   % /\  
   % forall(w in 1..num_workers) (
   %    sum([duration[j]*assignment[j,w] | j in 1..num_jobs]) <= earliest_end_time
   % )


   % using disjunctive/4
   % /\ forall(w in 1..num_workers) (
   %       disjunctive([start_time[j]*assignment[j,w] | j in 1..num_jobs ],
   %                   [duration[j]*assignment[j,w] | j in 1..num_jobs ],
   %                   [end_time[j]*assignment[j,w] | j in 1..num_jobs ],
   %                   [assignment[j,w] | j in 1..num_jobs ]
   %                   )
   % )

   % /\ 
   % forall(w in 1..num_workers) ( 
   %    disjunctive2([start_time[j]*assignment[j,w] | j in 1..num_jobs ],
   %                  [duration[j]*assignment[j,w] | j in 1..num_jobs ]) 
   %    % /\
   %    % diffn([start_time[j]*assignment[j,w] | j in 1..num_jobs ], 
   %    %       [end_time[j]*assignment[j,w] | j in 1..num_jobs ], 
   %    %       [duration[j]*assignment[j,w] | j in 1..num_jobs ],
   %    %       [1 | j in 1..num_jobs] 
   %    %      ) :: domain            
   % )

;   



%
% Set the lower bound of makespan x num_workers
%
constraint
   earliest_end_time*num_workers >= sum([duration[j]*resource[j] | j in 1..num_jobs]) 
;


%
% Handle precedences
%
constraint
   
   if do_precendences /\ num_precedences > 0 then
      forall(p in 1..num_precedences) (
          prec(precedences[p,1], precedences[p,2], start_time, duration)
      )
   else 
     true
   endif
;


%
% Handle idleness
% 
% If allow_idle = false then there can be no idle time 
% for any worker until his/her final task.
%
% We implement this simply as the checking that
% the total work time must be the same as the end_time for
% the last task.
%
constraint
  
   if allow_idle = false then
      forall(w in 1..num_workers) (
         max([ end_time[j]*bool2int(assignment[j, w] = 1) | j in 1..num_jobs]) =
              sum(j in 1..num_jobs) ( duration[j]*assignment[j, w] )
      )
   else 
     true
   endif

;

%
% Collect workers.
%
% Enforce that all jobs are done by "near" workers, i.e.
% seeing the workers as a "block".
%
% This is done with a (decomposition) of the global 
% constraint contiguity.
% (Cf http://hakank.org/minizinc/global_contiguity.mzn )
%
predicate global_contiguity(array[int] of var 0..1: x) =
  let { 
    int: lbx = min(index_set(x)),
    int: ubx = max(index_set(x)),
    var lbx..ubx: xstart,
    var lbx..ubx: xend
  }
  in
  forall(i in index_set(x)) (
     (i >= xstart /\ i <= xend) <-> x[i] = 1
  )
  /\ xend >= xstart
;


%
% Global contiguity via regular
% (Cf http://www.hakank.org/minizinc/contiguity_regular.mzn )
% 
predicate global_contiguity_regular(array[int] of var int: x) =
    let {
        int: n_states = 3,
        int: input_max = 2,
        int: initial_state = 1,
        set of int: accepting_states = {1,2,3},
        array[1..n_states, 1..input_max] of int: transition_fn =
             array2d(1..n_states, 1..input_max,
           [ 
              % note:all states are accepting states for
              % the regular expression 0*1*0*
              1,2, % state 1 (start): input 0 -> state 1, input 1 -> state 2 i.e. 0*
              3,2, % state 2: 1* 
              3,0, % state 3: 0* 
            ])
    }
    in 

    regular([x[i]+1|i in index_set(x)], n_states, input_max, transition_fn,
            initial_state, accepting_states) :: domain

;

constraint

   if collect_workers then
       forall(j in 1..num_jobs) (
          if resource[j] > 1 then
             % global_contiguity([assignment[j,w] | w in 1..num_workers])
             global_contiguity_regular([assignment[j,w] | w in 1..num_workers])
          else 
             true
          endif
       )
   else 
     true
   endif

;

%
% Data
%
% 
% See the data files scheduling_with_assignments*.dzn
% at http://www.hakank.org/minizinc/#scheduling_with_assignments
%

%
% Here is a simple example, just to show the structure of
% the data. It is the same as 
%    http://www.hakank.org/minizinc/scheduling_with_assignments1.dzn
%
% This problem instance is from Marriott & Stuckey: 
%   "Programming with constraints", page  112f)
% Furniture moving.
% Cf: http://www.hakank.org/minizinc/furniture_moving.mzn
%
% num_jobs = 4;
% num_workers = 4;
% duration = [30,10,15,15];
% resource = [3,1,3,2];
% max_time = 160; % optimal: 60
% allow_idle = true;
% collect_workers = false;
% do_precendences = false;
% num_precedences = 1;
% precedences = array2d(1..num_precedences, 1..2, 
% [ 
%   0,0 % dummy precedence
% ]);



output 
[ 
  "earliest_end_time: " ++ show(earliest_end_time) ++ "\n"
]
++
[
   "num_jobs   : " ++ show(num_jobs)    ++ "\n" ++
   "num_workers: " ++ show(num_workers) ++ "\n" ++
   "start_time : " ++ show(start_time)  ++ "\n" ++
   "duration   : " ++ show(duration)    ++ "\n" ++
   "end_time   : " ++ show(end_time)    ++ "\n" ++
   "resource   : " ++ show(resource)    ++ "\n" ++
   "allow_idle : " ++ show(allow_idle)  ++ "\n" ++
   "collect_workers : " ++ show(collect_workers)  ++ "\n" ++
   "do_precendences: " ++ show(do_precendences) ++ "\n" ++
   if do_precendences then
     "\nPrecedences:\n"
   else 
     ""
   endif
]
++
[
  if do_precendences then
      show(precedences[p,1]) ++ " before " ++ show(precedences[p,2]) ++ "\n"
   else 
     ""
   endif
   | p in 1..num_precedences
]
++ ["\nAssignment matrix (jobs/workers):"] ++
[
  if worker = 1 then "\nJob" ++ show(job) ++ ":" ++ 
      if job < 10 then " " else "" endif
      else " " endif ++
     show(assignment[job, worker])
  | job in 1..num_jobs, worker in 1..num_workers
]
++ ["\n\nAssignment matrix (workers/jobs):"] ++
[
  if job = 1 then "\nWorker" ++ show(worker) ++ ":" ++
     if worker < 10 then " " else "" endif
      else " " endif ++
     show(assignment[job, worker])
  | worker in 1..num_workers, job in 1..num_jobs
]
++ ["\n\nTime range for the jobs and the assigned worker(s):"] ++
[  
  if worker = 1 then "\nJob" ++ show(job) ++ 
          "(" ++ show(fix(start_time[job])) ++ ".." ++ show(fix(end_time[job])) ++ "): "
        else "" endif ++
     if fix(assignment[job, worker]) = 1 then
       show(worker) ++ " "
     else 
       ""
     endif
  | job in 1..num_jobs, worker in 1..num_workers

]
++ ["\n\nSchedule: jobs (workers):"] ++
[
    if time < fix(earliest_end_time) then 
      if job = 1 then 
           "\nTime: " ++ 
           show(time) ++ ": " ++ 
           if time < 10 then " " else "" endif ++
           if time < 100 then " " else "" endif ++
           " "
      else "" 
      endif 

      ++ % Note: This is hard to get in nice aligned columns
      if time >= fix(start_time[job]) /\ time < fix(end_time[job]) then
         "j" ++ show(job) ++ "(" ++ show([w | w in 1..num_workers where fix(assignment[job, w]) = 1]) ++ ")  " 
      else 
         " -        "
      endif 
    else 
         ""
    endif
   | time in 0..max_time, job in 1..num_jobs
]
++ ["\n\nSchedule: worker(job):"] ++
[
    if time < fix(earliest_end_time) then 
      if worker = 1 then 
           "\nTime: " ++ 
           show(time) ++ ": " ++ 
           if time < 10 then " " else "" endif ++
           if time < 100 then " " else "" endif ++
           " "
      else "" 
      endif 

      ++ % check which job (if any) the worker is assigned to at this time
      let {
         set of int: job = {j | j in 1..num_jobs where 
                               fix(assignment[j, worker]) = 1 /\ 
                               time >= fix(start_time[j])     /\ 
                               time < fix(end_time[j]) 
                           }
      }
      in 
      if card(job) > 0 then
         if set2array(job)[1] < 10 then " " else "" endif ++ % alignment of columns
         % This is cleaner (just the matrix of the job for each worker)
         show(set2array(job)[1]) ++ " "
      else 
         " - "
      endif

    else 
         ""
    endif
   | time in 0..max_time, worker in 1..num_workers
]
% Gantt like presentation of the workers
++ ["\n\nSchedule: worker(job), timeline: (earliest_end_time: " ++ show(earliest_end_time) ++ ")" ] ++
[
    if time < fix(earliest_end_time) then 
      if time = 0 then 
           "\nWorker: " ++ 
           show(worker) ++ ": " ++ 
           if worker < 10 then " " else "" endif ++
           " "
      else "" 
      endif 

      ++ % check which job (if any) the worker is assigned to at this time
      let {
         set of int: job = {j | j in 1..num_jobs where 
                               fix(assignment[j, worker]) = 1 /\ 
                               time >= fix(start_time[j])     /\ 
                               time < fix(end_time[j]) 
                           }
      }
      in 
      if card(job) > 0 then
         if set2array(job)[1] < 10 then " " else "" endif ++ % alignment of columns
         % This is cleaner (just the matrix of the job for each worker)
         show(set2array(job)[1]) ++ " "

         % show(worker) ++ "(" ++ show(job[1]) ++ ") " 
      else 
         " - "
      endif   
    else 
         ""
    endif


   | worker in 1..num_workers, time in 0..max_time
]
++ [ "\n\nTime:         " ] ++
[
  if time < fix(earliest_end_time) then
     let {
      int: mod10 = fix(time) mod 10
     } in
     if time > 0 /\ mod10 = 0 then 
        show(fix(time) mod 100) ++ " "
     else 
       show(fix(time) mod 10) ++ "  " 
     endif
  else
     ""
  endif
  | time in 0..max_time
]
;



