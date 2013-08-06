/*

  Scheduling speakers in AMPL+CP.

  From Rina Dechter, Constraint Processing, page 72
  Scheduling of 6 speakers in 6 slots.

  There are two solutions:
    [6, 3, 5, 2, 4, 1]
    [6, 4, 5, 2, 3, 1]


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n; # number of speakers
param m; # number of slots
param available{1..n, 1..m} binary;

var x{1..n} >= 1 <= n integer; # the allotted speaker slot

#
# constraints
#
s.t. c1: alldiff{i in 1..n} x[i];

s.t. c2{i in 1..n}:
    exists{j in 1..m: available[i,j] = 1}
       x[i] = j
;

data;

param n := 6;
param m := 6;
param available: 
     1 2 3 4 5 6 :=
 1   0 0 1 1 1 1    # {3,4,5,6},    step 2: the only one with 6 after speaker F -> 1
 2   0 0 1 1 0 0    # {3,4},        step 5: 3 or 4
 3   0 1 1 1 1 0    # {2,3,4,5},    step 3: only with 5 after F -> 1 and A -> 6
 4   0 1 1 1 0 0    # {2,3,4},      step 4: only with 2 after C -> 5 and F -> 1 
 5   0 0 1 1 0 0    # {3,4},        step 5: 3 or 4
 6   1 1 1 1 1 1    # {1,2,3,4,5,6} step 1: the only with 1
;
  


option solver gecode;
option gecode_options "var_branching=degree_size_min val_branching=min outlev=1 outfreq=1 timelimit=30";
# option solver ilogcp;

solve;

for{i in 1..n} {
  printf "%d ", x[i];
}
printf "\n";