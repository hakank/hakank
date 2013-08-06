/*

  Organize a day in AMPL+CP.

  Problem formulation:
  Slides on (finite domain) Constraint Logic Programming, page 38f

  http://www.icparc.ic.ac.uk/eclipse/reports/eclipse.ppt
  (via http://www.icparc.ic.ac.uk/eclipse/reports/ )


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;
param durations{1..n};

param b;
param before{1..b, 1..2};


var begins{1..n} >= 9 <= 17 integer;
var ends{1..n} >= 9 <= 17 integer;

#
# constraints
#
s.t. c1{i in 1..n}:
   ends[i] = begins[i] + durations[i];

s.t. c2{i in 1..n, j in 1..n: i < j}:
   begins[i]+durations[i] <= begins[j]
   or
   begins[j]+durations[j] <= begins[i]
;

s.t. c3{t in 1..b}:
   ends[before[t,1]] <= ends[before[t,2]];

s.t. c4: begins[1] >= 11;


data;

param n := 4;
param durations :=
  1  4
  2  1
  3  2
  4  1
;  

param b := 2;
param before: 1 2 :=
  1  4 3
  2  2 1
;

option solver gecode;
# option gecode_options "var_branching=degree_size_min val_branching=min outlev=1 outfreq=1 timelimit=30";
# option solver ilogcp;

solve;

for{i in 1..n} {
  printf "task %d: %2d..%2d\n", i, begins[i], ends[i];
     
}