/*

  Set covering problem in AMPL+CP.

  This example is from the OPL example covering.mod
  """
  Consider selecting workers to build a house. The construction of a 
  house can be divided into a number of tasks, each requiring a number of 
  skills (e.g., plumbing or masonry). A worker may or may not perform a 
  task, depending on skills. In addition, each worker can be hired for a 
  cost that also depends on his qualifications. The problem consists of 
  selecting a set of workers to perform all the tasks, while minimizing the 
  cost. This is known as a set-covering problem. The key idea in modeling 
  a set-covering problem as an integer program is to associate a 0/1 
  variable with each worker to represent whether the worker is hired. 
  To make sure that all the tasks are performed, it is sufficient to 
  choose at least one worker by task. This constraint can be expressed by a 
  simple linear inequality.
  """

  Solution from the OPL model:
  """
  Optimal solution found with objective: 14
  crew= {23 25 26}
  """


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param NumWorkers;
set Workers = 1..NumWorkers;
param NumTasks;
param Qualified{1..NumTasks, Workers} default 0;
param Cost{Workers};

# Which worker to hire
var Hire{Workers} binary;
# Total cost
var TotalCost >= 0 <= (NumWorkers*max{w in Workers} Cost[w]) integer;

minimize obj: TotalCost;

#
# constraints
#
s.t. c1{t in 1..NumTasks}:
   sum{w in Workers: Qualified[t,w] > 0} Hire[w] >= 1
;

s.t. c2: TotalCost = sum{w in Workers} Cost[w]*Hire[w];

data;

param NumWorkers := 32;
param NumTasks := 15;

# cost per worker
param Cost :=
 1 1
 2 1
 3 1
 4 1
 5 1
 6 1
 7 1
 8 1
 9 2
10 2
11 2
12 2 
13 2
14 2
15 2
16 3
17 3
18 3
19 3
20 4
21 4
22 4
23 4
24 5
25 5
26 5
27 6
28 6
29 6
30 7
31 8
32 9
;

param Qualified: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 :=
 1  1  .  .  .  .  .  .  .  9  .  .  .  .  .  .  .  .  . 19  .  . 22  .  . 25  .  . 28  .  . 31  .
 2   . 2  .  .  .  .  .  .  .  .  . 12  .  . 15  .  .  . 19  . 21  . 23  .  .  . 27  . 29 30 31 32
 3   .  . 3  .  .  .  .  .  . 10  .  .  .  .  .  .  .  . 19  .  .  .  . 24  . 26  .  .  . 30  . 32
 4   .  .  . 4  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  . 21  .  .  . 25  .  . 28  .  .  . 32
 5   .  .  .  . 5  .  .  .  .  . 11  .  .  .  . 16  .  .  .  .  . 22 23  .  .  . 27  .  .  . 31  . 
 6   .  .  .  .  . 6  .  .  .  .  .  .  .  .  .  .  .  .  . 20  .  .  . 24  . 26  .  .  . 30  . 32
 7   .  .  .  .  .  . 7  .  .  .  . 12  .  .  .  . 17  .  .  .  .  .  .  . 25  .  .  .  . 30 31  .
 8   .  .  .  .  .  .  . 8  .  .  .  .  .  .  .  . 17  .  . 20  . 22 23  .  .  .  .  .  .  .  .  .
 9   .  .  .  .  .  .  . .  9  .  .  . 13 14  .  .  .  .  .  .  .  .  .  .  . 26  .  . 29 30 31  .
10   .  .  .  .  .  .  . .  . 10  .  .  .  .  .  .  .  .  .  . 21  .  .  . 25  .  .  .  .  . 31 32 
11   .  .  .  .  .  .  . .  .  .  .  .  . 14 15  .  . 18  .  .  .  . 23 24  .  . 27  .  . 30  . 32 
12   .  .  .  .  .  .  . .  .  .  .  .  .  .  .  .  . 18 19  .  . 22  . 24  . 26  .  . 29  . 31  .
13   .  .  .  .  .  .  . .  .  . 11  .  .  .  .  .  .  .  . 20  .  .  .  . 25  .  . 28  . 30  . 32
14   .  .  .  .  .  .  . .  .  .  .  .  .  .  . 16  .  . 19  .  .  . 23  .  .  .  .  .  .  .  31 .
15   .  .  .  .  .  .  . .  9  .  .  .  .  .  .  .  . 18  .  .  .  .  .  .  . 26  . 28  .  . 31 32 
;

# This is not a CP model
# option solver cplex;

option solver gecode;
option gecode_options "var_branching=degree_size_max val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=auto debugexpr=0 logperiod=1 logverbosity=0";


solve;

# display Hire;

printf "TotalCost: %d\n", TotalCost;
printf "Workers to hire: ";
for{w in Workers} {
  if Hire[w] = 1 then {
        printf "%2d ", w;
  }
}

printf "\n";
