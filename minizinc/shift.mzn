% 
% Global constraint shift in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cshift.html
% """
% shift (MIN_BREAK, MAX_RANGE, TASKS)
%
% Purpose
% 
% The difference between the end of the last task of a shift and the origin 
% of the first task of a shift should not exceed the quantity MAX_RANGE. 
% Two tasks t1 and t2 belong to the same shift if at least one of the 
% following conditions is true:
%
%  * Task t2 starts after the end of task t1 at a distance that is less 
%    than or equal to the quantity MIN_BREAK,
%  * Task t1 starts after the end of task t2 at a distance that is less 
%    than or equal to the quantity MIN_BREAK.
%  * Task t1 overlaps task t2.

% Example
%     (
%     6, 8, <
%     id-1 origin-17 end-20, 
%     id-2 origin-7  end-10, 
%     id-3 origin-2  end-4, 
%     id-4 origin-21 end-22, 
%     id-5 origin-5  end.6
%     〉
%     )
%
%  Figure 4.249.1 represents the different tasks of the example. Each task 
% is drawn as a rectangle with its corresponding id attribute in the middle. 
% We indicate the distance between two consecutive tasks of a same shift and 
% observe that it is less than or equal to MIN_BREAK=6. Since each shift has 
% a range that is less than or equal to MAX_RANGE=8, the shift constraint 
% holds (the range of a shift is the difference between the end of the last 
% task of the shift and the origin of the first task of the shift).
% 
% [See the figure at https://sofdem.github.io/gccat/gccat/Cshift.html
%  which states the task ids of the shifts and their order
%
%      first shift             second shift
%      range = 8    break = 7   range = 5
%
%       3   5   2                 1     4
%       --  -   ---               ----  -
% time: 2   5   7                 17    21
%  
% ]
% 
% """

% [


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n =  5;
int: num_shifts = 2;
int: max_time = 22;
array[1..n, 1..2] of var 1..max_time: tasks; % origin, end
array[1..n] of var 1..num_shifts: shifts; % identify the shift for each task id
var int: min_break;
var int: max_range;


%
% I added num_shifts and shifts as a parameter
%
predicate shift(var int: min_break, var int: max_range, array[int, 1..2] of var int: tasks, int: num_shifts, array[int] of var int: shifts) =
   let {
     int: n = card(index_set_1of2(tasks)),
     int: lbt = min(index_set_1of2(tasks)),
     int: ubt = max(index_set_1of2(tasks))
   }
   in

   % sanity check: task must start must before it ends
   forall(i in lbt..ubt) (
     tasks[i,1] < tasks[i,2]
   )
   /\
   min_break > 0
   /\
   max_range >= 0
   /\
   max_range < ub_array(tasks)

   /\ %  * Task t2 starts after the end of task t1 at a distance that is less 
      %    than or equal to the quantity MIN_BREAK,
      %  * Task t1 starts after the end of task t2 at a distance that is less 
      %    than or equal to the quantity MIN_BREAK.
      %  * Task t1 overlaps task t2.
   forall(i, j in lbt..ubt where i < j) (
      
      (shifts[i] = shifts[j] <- 
              (
              tasks[j, 1 ]- tasks[i,2] >= 0
              /\
              tasks[j, 1] - tasks[i,2] <= min_break
              )
             )
      /\
      (shifts[i] = shifts[j] <- 
              (
              tasks[i, 1 ]- tasks[j,2] >= 0
              /\
              tasks[i, 1] - tasks[j,2] <= min_break
              ) )
      /\ % overlaps
      (shifts[i] = shifts[j] <-
                  (
                    (tasks[i, 2] >= tasks[j,1] /\
                    tasks[i, 2] <= tasks[j,2])
                    \/
                    (tasks[j, 2] >= tasks[i,1] /\
                    tasks[j, 2] <= tasks[i,2])
                  )
     )
    )

    /\ % the shift must have a range <= max_range 
    forall(k in 1..num_shifts) (
      let {
         var int: min_start,
         var int: max_end,
         array[1..n] of var 0..max_time: tasks_start,
         array[1..n] of var 0..max_time: tasks_end
      }
      in
      forall(t in lbt..ubt) (
        (
         (tasks_start[t] = tasks[t, 1] <-> shifts[t] = k)
         /\
         (tasks_start[t] = ub_array(tasks) <-> shifts[t] != k)
        )
        /\
        (
         (tasks_end[t] = tasks[t,2] <-> shifts[t] = k)
         /\
         (tasks_end[t] = 0 <-> shifts[t] != k)
        )
       /\
       minimum(min_start, tasks_start)
       /\
       maximum(max_end, tasks_end)
       /\
       max_end > min_start
       /\
       max_end - min_start <= max_range
     )
   )

   /\ % the task with the minimum start should have shift 1
   let {
      var lbt..ubt: min_ix
   }
   in
   minimum(min_ix, [tasks[j, 1] | j in lbt..ubt])
   /\
   shifts[min_ix] = 1
;

predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

% solve satisfy;
solve :: int_search([tasks[i, j] | i in index_set_1of2(tasks), j in 1..2] ++ shifts ++ [min_break, max_range], first_fail, indomain_min, complete) satisfy; 

constraint
  cp2d(tasks, array2d(1..n, 1..num_shifts, 
     [
       17, 20, % task 1
        7, 10, % task 2 
        2,  4, % task 3 
       21, 22, % task 4
        5,  6,  % task 5
     ]))

   /\
   min_break = 6
   /\
   max_range = 8
    
   /\
   shift(min_break, max_range, tasks, num_shifts, shifts)

   % the shifts in the example should be
   %  tasks: 1 2 3 4 5 
   %  shift: 2 1 1 2 1
   % /\
   % cp1d(shifts, [2,1,1,2,1])

;


output 

[ "tasks:"] ++
[
  if j = 1 then "\n" ++ show(i) ++ ": "  else " " endif ++
    show(tasks[i,j])
  | i in 1..n, j in 1..2
] ++ 
[
  "\nshifts: ", show(shifts), "\n",
  "min_break: ", show(min_break), "\n",
  "max_range: ", show(max_range), "\n",
  
]
;
