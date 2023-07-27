% 
% Global constraint sliding_time_window in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Csliding_time_window.html
% """
% sliding_time_window(WINDOW_SIZE,LIMIT,TASKS)
% 
% Purpose
%
% For any time window of size WINDOW_SIZE, the intersection of all the tasks 
% of the collection TASKS with this time window is less than or equal to a 
% given limit LIMIT.
%
% Example
%     (
%     9,6, <
%     id-1 origin-10 duration-3,
%     id-2 origin-5  duration-1,
%     id-3 origin-6  duration-2,
%     id-4 origin-14 duration-2,
%     id-5 origin-2  duration-2
%     >
%     )
%
% The lower part of Figure 4.255.1 indicates the different tasks on the 
% time axis. Each task is drawn as a rectangle with its corresponding 
% identifier in the middle. Finally the upper part of Figure 4.255.1 shows 
% the different time windows and the respective contribution of the tasks 
% in these time windows. A line with two arrows depicts each time window. 
% The two arrows indicate the start and the end of the time window. At the 
% right of each time window we give its occupation. Since this occupation 
% is always less than or equal to the limit 6, the sliding_time_window 
% constraint holds.
%
% 
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n;
int: max_time;
int: window_size;
var int: limitx;
array[1..n, 1..2] of var 1..max_time: tasks;
array[1..max_time] of var int: sums;

%
% I added max_time
%
predicate sliding_time_window(int: window_size, 
                              var int: limitx, 
                              array[int, 1..2] of var int: tasks,
                              int: max_time) =
   let {
      int: lbt = min(index_set_1of2(tasks)),
      int: ubt = min(index_set_1of2(tasks)),
      array[1..max_time] of var 0..ubt: occupied

   }
   in
   % how many tasks occupies this time entry
   forall(i in 1..max_time) (
      occupied[i] = sum(j in lbt..ubt) (
         bool2int(
             i >= tasks[j, 1] /\ i < tasks[j, 1] + tasks[j, 2]
         )
      )
   )
   /\ % sliding sum over occupied
   forall(i in 1..max_time) (
      sums[i] = sum(j in i..i+window_size-1 where j <= max_time) (
          occupied[j]
      )
      /\
     sums[i] <= limitx
   )
   /\ % all sums must be less or equal the limit
   forall(i in index_set_1of2(tasks)) (
     tasks[i, 1] + tasks[i,2] <= max_time
   )
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

solve satisfy;
% solve :: int_search([tasks[i,j] | i in 1..n, j in 1..2] ++ [limitx] ++ sums, "first_fail", "indomain", "complete") satisfy;

constraint
    limitx = 6 
    /\
    cp2d(tasks, array2d(1..n, 1..2, 
       [
          10, 3,
           5, 1,
           6, 2,
           14, 2,
           2, 2,
       ]))
   /\
   sliding_time_window(window_size, limitx, tasks, max_time)
  
   % /\    % 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6  
   % sums = [_,6,_,_,6,6,_,_,_,5,_,_,_,2,_,_] % the sums of the example

;

%
% data
%
n = 5;
max_time = 16;
window_size = 9;


output 
[ 
  "tasks:"
] ++
[
  if j = 1 then "\n  " else " " endif ++
    show(tasks[i,j])
  | i in 1..n, j in 1..2
] ++ 
[
  "\nsums: ", show(sums), "\n",
  "limit: ", show(limitx), "\n"
]
;
