% 
% Global constraint sliding_time_window_from_start in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Csliding_time_window_from_start.html
% """
% sliding_time_window_from_start(WINDOW_SIZE,LIMIT,TASKS,START)
%
% Purpose
%
% The sum of the intersections of all the tasks of the TASKS collection 
% with interval​[START, START+WINDOW_SIZE-1] is less than or equal to LIMIT.
% 
% Example
%     (
%     9,6,<
%     id-1 origin-10 duration-3,
%     id-2 origin-5  duration-1,
%     id-3 origin-6  duration-2
%     >,5
%     )
%
% The intersections of tasks id-1 origin-10 duration-3, 
%  id-2 origin-5 duration-1, and id-3 origin-6 duration-2 with interval 
% [START, START+WINDOW_SIZE-1] = [5, 5+9-1] = [5, 13] are respectively 
% equal to 3, 1, and 2 (i.e., the three tasks of the TASKS collection are 
% in fact included within interval [5, 13]). Consequently, the 
% sliding_time_window_from_start constraint holds since the sum 3+1+2 
% of these intersections does not exceed the value of its second 
% argument LIMIT=6.
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 3;
int: max_time = 13;
array[1..n, 1..2] of var 1..max_time: tasks;
int: window_size = 9;
var int: limitx;
int: start = 5;
array[start..start+window_size-1] of var 0..n: occupied;

predicate sliding_time_window_from_start(
                                   int: window_size, 
                                   var int: limitx, 
                                   array[int, 1..2] of var int: tasks, 
                                   int: start) =

   % how many tasks occupies this time entry
   forall(i in start..start+window_size-1) (
      occupied[i] = sum(j in min(index_set_1of2(tasks))..max(index_set_1of2(tasks))) (
         bool2int(
             i >= tasks[j, 1] /\ i < tasks[j, 1] + tasks[j, 2]
         )
      )
   )
  /\
  limitx >=  sum(occupied)
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

constraint
  limitx = 6
  /\
  cp2d(tasks, array2d(1..n, 1..2, 
     [
       10,3,
        5,1,
        6,2
     ]))
   /\
   sliding_time_window_from_start(window_size, limitx, tasks, start)
;


output [
"window_size: " ++ show(window_size) ++ "\n"++
"limitx: " ++ show(limitx) ++ "\n"++
"tasks: " ++ show(tasks) ++ "\n"++
"start: " ++ show(start) ++ "\n" ++
"occupied: " ++ show(occupied) ++ "\n"
];
