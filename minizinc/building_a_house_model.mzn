% 
% Scheduling (Building a house) in MiniZinc.
% 
% A more general model of building a house (or other similar scheduling
% tasks).
% 
% This is a generalization of the two models:
%   http://www.hakank.org/minizinc/building_a_house.mzn
%   http://www.hakank.org/minizinc/building_a_house2.mzn
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

int: num_tasks;

% Defining this string array in a .dzn file throws an error with mzn2fzn:
%    "Array index ranges do not match."
% so I skip it...
% array[1..num_tasks] of string: tasks;

array[1..num_tasks] of int: duration;
array[1..num_tasks] of int: height;
int: total_duration = sum(t in 1..num_tasks) (duration[t]);

% precendeces
int: num_precedences;
array[1..num_precedences, 1..2] of int: precedences;

% variables
array[1..num_tasks] of var 1..total_duration: start;
array[1..num_tasks] of var 1..total_duration: end;
var 0..sum(height): limitx;
var 1..total_duration: makespan;

% handle the precedences
predicate prec(int:x , int: y, array[int] of var int: s, array[int] of var int: d) =
   s[x] + d[x] <= s[y]
;

solve minimize makespan;
% solve :: int_search(
%         start ++ [makespan, limitx], 
%         first_fail,
%         indomain_min, 
%         complete) 
%     % satisfy;
%     minimize makespan;


constraint
  % to be minimized
  maximum(makespan, end)/\
  cumulative(start, duration, height, limitx) /\

  forall(t in 1..num_tasks) (
     end[t] = start[t] + duration[t]
  )
  /\
 
  % precedences
  forall(p in 1..num_precedences) (
     prec(precedences[p,1], precedences[p,2], start, duration)
  )
;


% Nicer output, only with the minizinc solver
output [
  "makespan: " ++ show(makespan) ++ "\n" ++ 
  "limitx: " ++ show(limitx) ++ "\n"
] ++
[
  % show(tasks[t]) ++ ": " ++ show(start[t]) ++ " - " ++ show(duration[t]) ++ " --> " ++ show(start[t] + duration[t]) ++ "\n"
  show(t) ++ ": " ++ show(start[t]) ++ " - " ++ show(duration[t]) ++ " --> " ++ show(start[t] + duration[t]) ++ "\n"
  | t in 1..num_tasks
] ++ ["\n"];
