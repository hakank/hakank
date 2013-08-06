/*

  Cumulative scheduling in AMPL+CP.

  Example from the Sisctus documentation:
  http://www.sics.se/sicstus/docs/latest/html/sicstus/Cumulative-Scheduling.html#Cumulative%20Scheduling

  """
  Cumulative Scheduling

  This example is a very small scheduling problem. We consider seven
  tasks where each task has a fixed duration and a fixed amount of used
  resource:

  Task Duration Resource
   t1    16       2
   t2     6       9
   t3    13       3
   t4     7       7
   t5     5      10
   t6    18       1
   t7     4      11

  The goal is to find a schedule that minimizes the completion time for
  the schedule while not exceeding the capacity 13 of the resource. The
  resource constraint is succinctly captured by a cumulative/4
  constraint. Branch-and-bound search is used to find the minimal
  completion time. 

  This example was adapted from [Beldiceanu & Contejean 94]. 
  """

  Note: This model simulates the cumulative constraint.

  There are two solutions with capacity 13, where the end time is 23:
    Start: [7, 1, 1, 7, 14, 1, 19]
    Start: [1, 17, 10, 10, 5, 5, 1]



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n integer > 0;
param m integer > 0;
param duration{1..n};
param resource{1..n};
param capacity;

# decision variables
var start_times{1..n} >= 1 <= m integer;
var end_times{1..n} >= 1 <= m integer;
var last_time >= 1 <= m integer;


minimize obj: last_time;

#
# constraints
#
s.t. c1{i in 1..n}:  end_times[i] = start_times[i] + duration[i];
s.t. c2{t in 1..n}: last_time >= end_times[t];

# "cumulative"
s.t. c3{t in 0..m}:
     capacity >= sum{task in 1..n} (resource[task]*(if t >= start_times[task] and t < end_times[task] then 1))
;


data;

param n := 7;
param m := 50;
param capacity := 13;
param: duration resource :=  
   1  16  2
   2   6  9 
   3  13  3
   4   7  7
   5   5 10
   6  18  1
   7   4 11
;


option show_stats 2;
# option presolve 0;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=auto alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display start_times, duration, end_times, resource;
display last_time;

printf "\nOrder of tasks:\n";
for{t in 1..m: t <= last_time} {
    printf "Time %d:\n", t;
    for{task in 1..n} {
       if t >= start_times[task] and t < end_times[task] then
          printf "\ttask %d (res:%d)\n", task, resource[task]; 
    }
    printf "\tTotal resources: %2d\n", sum{task in 1..n: t >= start_times[task] and t < end_times[task]} resource[task];
  
}

printf "\n";