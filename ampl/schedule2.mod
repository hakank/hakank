/*

  Scheduling problem in AMPL+CP.

  From Dennis E. Sasha "Puzzles for Programmers and Pros",
  page 131f
  """
  In which order do you schedule the tasks starting from current
  day 0?
  Task  T1 takes 4 days with deadline on day 45
  Task  T2 takes 4 days with deadline on day 48
  Task  T3 takes 5 days with deadline on day 25
  Task  T4 takes 2 days with deadline on day 49
  Task  T5 takes 5 days with deadline on day 36
  Task  T6 takes 2 days with deadline on day 31
  Task  T7 takes 7 days with deadline on day 9
  Task  T8 takes 5 days with deadline on day 39
  Task  T9 takes 4 days with deadline on day 13
  Task T10 takes 6 days with deadline on day 17
  Task T11 takes 4 days with deadline on day 29
  Task T12 takes 1 days with deadline on day 19
  """
  
  The answer (according to page 132) is:

  T7 T9 T10 T12 T3 T11 T6 T5 T8 T1 T2 T4


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n integer > 0;
param duration{1..n};
param deadline{1..n};
param max_deadline = max{i in 1..n} deadline[i];

# decision variables
var start_times{1..n} >= 0 <= max_deadline integer;
var end_times{1..n} >= 0 <= max_deadline integer;
var last_time >= 0 <= max_deadline integer;

minimize obj: last_time;

#
# constraints
#
s.t. c1: last_time = max{i in 1..n} end_times[i];

s.t. c2{i in 1..n}:
      end_times[i] = start_times[i] + duration[i] and
      end_times[i] <= deadline[i]
;

# disjunctive
s.t. c3{t1 in 1..n, t2 in 1..n: t1 != t2}:
     end_times[t1] <= start_times[t2] or
     start_times[t1] >= end_times[t2] 
;


data;

param n := 12;
param: duration deadline :=  
1  4 45
2  4 48
3  5 25
4  2 49
5  5 36
6  2 31
7  7  9
8  5 39
9  4 13
10 6 17
11 4 29
12 1 19 
;


option show_stats 2;
# option presolve 0;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=med outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display start_times, duration, end_times, deadline;
display last_time;

printf "\nOrder of tasks:\n";
for{t in 0..max_deadline} {
  printf "Time %2d: ", t;
  for{task in 1..n} {
    if t >= start_times[task] and t < end_times[task] then
       printf "task %2d\n", task; 
  }
  
}

printf "\n";