/*

  Simple PERT model in AMPL+CP.

  From Pascal van Hentenryck 
  "Scheduling and Packing In the Constraint Language cc(FD)", page 7f
  http://citeseer.ist.psu.edu/300151.html
  

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param maxTime;
param n;
param numDependencies;
param Times{1..n};
param Dependencies{1..numDependencies,1..2};

# decision variables
var Start{1..n} >= 0 <= maxTime integer;
var SumTimes = sum{i in 1..n} Start[i];

minimize obj: SumTimes;
# minimize obj: Start[n];

#
# constraints
#
s.t. c1{i in 1..numDependencies}:
      Start[Dependencies[i,1]] >= (Start[Dependencies[i,2]] + Times[Dependencies[i,2]])
;

data;

param n := 11;
param maxTime := 30;
param Times :=
 1 7
 2 3
 3 1
 4 8
 5 1
 6 1
 7 1
 8 3
 9 2
10 1
11 1
;

param numDependencies := 15;
# Note: There is no Si
param Dependencies: 1 2 :=
 1  2 1   # Sb >= Sa + 7
 2  4 1   # Sd >= Sa + 7
 3  3 2   # Sc >= Sb + 3
 4  5 3   # Se >= Sc + 1
 5  5 4   # Se >= Sd + 8
 6  7 3   # Sg >= Sc + 1
 7  7 4   # Sg >= Sd + 8
 8  6 4   # Sf >= Sd + 8
 9  6 3   # Sf >= Sc + 1
10  8 6   # Sh >= Sf + 1
11  9 8   # Sj >= Sh + 3
12  10 7  # Sk >= Sg + 1
13  10 5  # Sk >= Se + 1
14  10 9  # Sk >= Sj + 2
15  11 10 # Send >= Sk + 1
;


option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display Start;
display SumTimes;
display Start[n];

