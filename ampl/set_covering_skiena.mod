/*

  Set covering problem in AMPL.

  Example from Steven Skiena, The Stony Brook Algorithm Repository
  http://www.cs.sunysb.edu/~algorith/files/set-cover.shtml
  """
  Input Description: A set of subsets S_1, ..., S_m of the 
  universal set U = {1,...,n}.
  
  Problem: What is the smallest subset of subsets T subset S such that \cup_{t_i in T} t_i = U?
  """
  Data is from the pictures INPUT/OUTPUT.

  Solution: Sets 3,6,7. Total elements choosen: 15
  Another solution with three sets is {4,6,7}, but the total choosen elements is 17.


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param num_sets;
param num_elements;
param belongs{1..num_sets, 1..num_elements} binary default 0;
param set_partition;

var x{1..num_sets} binary;
# total number of elements in the choosen sets
var tot_elements >= 0 integer;
var z >= 0  integer;

minimize obj: z;
# minimize obj: tot_elements;
# minimize obj: tot_elements + z;

#
# constraints
#

# (Manually select set covering or set partition)
# s.t. c1{j in 1..num_elements}: 
#    # sum{i in 1..num_sets} belongs[i,j]*x[i] = 1
#    sum{i in 1..num_sets} belongs[i,j]*x[i] >= 1
#;

# Do it via the flag set_partition
s.t. c1b{j in 1..num_elements}: 
   set_partition = 1 ==> 
     sum{i in 1..num_sets} belongs[i,j]*x[i] = 1
   else 
     sum{i in 1..num_sets} belongs[i,j]*x[i] >= 1
;

s.t. c2: z = sum{i in 1..num_sets} x[i];
s.t. c3: tot_elements = sum{i in 1..num_sets, j in 1..num_elements} x[i]*belongs[i,j];


data;

param num_sets := 7;
param num_elements = 12;
param set_partition := 0;

param belongs: 1 2 3 4 5 6 7 8 9 10 11 12:=
    #  1 2 3 4 5 6 7 8 9 0 1 2  elements
1      1 1 . . . . . . . . . .       # Set 1
2      . 1 . . . . . 1 . . . .       #     2
3      . . . . 1 1 . . . . . .       #     3
4      . . . . . 1 1 . . 1 1 .       #     4
5      . . . . . . . . 1 1 . .       #     5
6      1 1 1 . 1 . . . 1 1 1 .       #     6
7      . . 1 1 . . 1 1 . . 1 1       #     7
;

option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

# option solver cplex;

solve;


if set_partition = 1 then {
  printf "\nSet partition problem.\n";
} else {
  printf "\nSet covering problem.\n";
}

display x;
display tot_elements;
display z;

print "Selected sets:";
for{i in 1..num_sets} {
   if x[i] = 1 then {
      printf "%d: ", i;
      print {j in 1..num_elements} belongs[i,j];
   }

}