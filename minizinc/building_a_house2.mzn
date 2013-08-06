% 
% Scheduling (Building a house) in MiniZinc.
% 
% This problem is from Mozart/Oz 
% http://www.mozart-oz.org/home/doc/fdt/node47.html#section.scheduling.house
% """
% The task names, their description, duration (in days) and the company in 
% charge are given in Figure 11.1. For example, b denotes the task involved 
% with the carpentry for the roof. This task lasts for 3 days. Task a must be 
% finished before the work for task b is started (indicated by the column 
% Predecessor). The company in charge for task b is House Inc. The overall 
% goal is to build the house as quickly as possible.
%
% Task Description        Duration Predecessor Company
%  a   Erecting Walls     7        none        Construction Inc.
%  b   Carpentry for Roof 3        a           House Inc.
%  c   Roof               1        b           House Inc.
%  d   Installations      8        a           Construction Inc.
%  e   Facade Painting    2        c, d        Construction Inc.
%  f   Windows            1        c, d        House Inc. 
%  g   Garden             1        c, d        House Inc.
%  h   Ceilings           3        a           Construction Inc.
%  i   Painting           2        f, h        Builder Corp.
%  j   Moving in          1        i           Builder Corp.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

% For output[]
int: num_tasks = 10;

array[1..num_tasks] of string: tasks = 
                               ["erecting_walls",
                               "carpentry_for_roof",
                               "roof",
                               "installations",
                               "facade_painting",
                               "windows",
                               "garden",
                               "ceilings",
                               "painting",
                               "moving_in"];

% for the precedences
int: erecting_walls     = 1;
int: carpentry_for_roof = 2;
int: roof               = 3;
int: installations      = 4;
int: facade_painting    = 5;
int: windows            = 6;
int: garden             = 7;
int: ceilings           = 8;
int: painting           = 9;
int: moving_in          = 10;

array[1..num_tasks] of int: duration =  [ 7, 3, 1, 8, 2, 1, 1, 3, 2, 1];
array[1..num_tasks] of int: height   =  [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1];
int: total_duration = sum(t in 1..num_tasks) (duration[t]);

% precendeces
int: num_precedences = 13;
array[1..num_precedences, 1..2] of int: precedences;

% variables
array[1..num_tasks] of var 1..total_duration: start;
array[1..num_tasks] of var 1..total_duration: end;
var 1..3: limitx;
var 1..total_duration: makespan;

% handle the precedences
predicate prec(int:x , int: y, array[int] of var int: s, array[int] of var int: d) =
   s[x] + d[x] <= s[y]
;

% solve minimize makespan;
solve :: int_search(
        start ++ [makespan, limitx], 
        % start ++ [makespan], 
        first_fail,
        indomain_min, 
        complete) 
    % satisfy;
    minimize makespan;


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

precedences = array2d(1..num_precedences, 1..2, 
[
% a   b
erecting_walls, carpentry_for_roof, 
%  b   c
carpentry_for_roof, roof,
%  a   d
erecting_walls, installations,
%  c   e
roof, facade_painting,
%  d   e
installations, facade_painting,
%  c   f
roof, windows,
%  d   f
installations, windows,
%  c   g
roof, garden,
%  d   g
installations, garden,
%  a   h
erecting_walls, ceilings,
%  f   i
windows, painting,
%  h   i
ceilings, painting,
%  i   j
painting, moving_in
]);

% Nice output, only with the minizinc solver
output [
  "makespan: " ++ show(makespan) ++ "\n"
] ++
[
  show(tasks[t]) ++ ": " ++ show(start[t]) ++ " - " ++ show(duration[t]) ++ " --> " ++ show(start[t] + duration[t]) ++ "\n"
  | t in 1..num_tasks
] ++ ["\n"];
