/*

  Place number puzzle in AMPL+CP.

  http://ai.uwaterloo.ca/~vanbeek/Courses/Slides/introduction.pdf
  """
  Place numbers 1 through 8 on nodes
  - each number appears exactly once
  - no connected nodes have consecutive numbers
       2 - 5 
     / | X | \
   1 - 3 - 6 - 8
     \ | X | /
       4 - 7
  """
  
  Two solutions (plus their reverses):
  x = [2, 5, 8, 6, 3, 1, 4, 7]
  x = [2, 6, 8, 5, 4, 1, 3, 7]


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param m;
param graph{1..m, 1..2};

var x{1..n} >= 1 <= n integer;

#
# constraints
#
s.t. c1: alldiff{i in 1..n} x[i];

s.t. c2{j in 1..m}: abs(x[graph[j,1]] -x [graph[j,2]]) > 1;

# symmetry breaking
# s.t. c3: x[1] < x[n];

data;

param n := 8;
param m := 32;

param graph: 1 2 :=
   1 1 2 
   2 1 3 
   3 1 4 
   4 2 1 
   5 2 3 
   6 2 5 
   7 2 6 
   8 3 2 
   9 3 4 
  10 3 6 
  11 3 7 
  12 4 1 
  13 4 3 
  14 4 6 
  15 4 7 
  16 5 2 
  17 5 3 
  18 5 6 
  19 5 8 
  20 6 2 
  21 6 3 
  22 6 4 
  23 6 5 
  24 6 7 
  25 6 8 
  26 7 3 
  27 7 4 
  28 7 6 
  29 7 8 
  30 8 5 
  31 8 6 
  32 8 7
;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=60";

# option solver ilogcp;

solve;
# write gtest;

for {i in 1..n} {
    printf "%d ", x[i];
};
printf "\n";