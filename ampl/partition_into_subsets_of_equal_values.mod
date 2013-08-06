/*

  Partition into subset of equal sums in AMPL+CP.

  From Programmers Stack Exchange (C#)
  http://programmers.stackexchange.com/questions/153184/partitioning-set-into-subsets-with-respect-to-equality-of-sum-among-subsets
  Partitioning set into subsets with respect to equality of sum among subsets
  """
  let say i have {3, 1, 1, 2, 2, 1,5,2,7} set of numbers, I need to split the 
  numbers such that sum of subset1 should be equal to sum of subset2 
  {3,2,7} {1,1,2,1,5,2}. First we should identify whether we can split number(one 
  way might be dividable by 2 without any remainder) and if we can, we should 
  write our algorithm two create s1 and s2 out of s.
  
  How to proceed with this approach? I read partition problem in wiki and even in some 
  articles but i am not able to get anything. Can someone help me to find the 
  right algorithm and its explanation in simple English?
  """


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;
param num_subsets;

param s{1..n};
param the_sum = (sum{i in 1..n} s[i]) / num_subsets;

var x{1..n} >= 1 <= num_subsets integer;

check: (sum{i in 1..n} s[i]) mod 2 = 0;

#
# constraints
#

# Some variants
# s.t. c0{p in 1..num_subsets-1}:
#        (sum{i in 1..n} (if x[i] == p then 1)*s[i]) =
#        (sum{i in 1..n} (if x[i] == p+1 then 1)*s[i])
# ;

# Variant (hardcoded for 2 subsets)
# s.t. c0: (sum{i in 1..n} (if x[i] == 1 then 1)*s[i]) =
#          (sum{i in 1..n} (if x[i] == 2 then 1)*s[i])
# ;

# This checks the sums separately
# s.t. c1{p in 1..num_subsets}:
#      sums[p] = sum{i in 1..n} (if x[i] == p then 1)*s[i]
# ;

# s.t. c2{p in 1..num_subsets-1}:
#      sums[p] = sums[p+1]
# ;

s.t. c1{p in 1..num_subsets}:
     the_sum = sum{i in 1..n} (if x[i] == p then 1)*s[i]
;

# symmetry breaking
s.t. c3: x[1] = 1;

# data partition_into_subsets_of_equal_values1.dat;
# data partition_into_subsets_of_equal_values2.dat;
data partition_into_subsets_of_equal_values3.dat;
# data partition_into_subsets_of_equal_values4.dat; # hard 

# data;

# param n := 9;
# param num_subsets := 2;
# param s := 
#    1 3
#    2 1
#    3 1
#    4 2
#    5 2
#    6 1 
#    7 5 
#    8 2
#    9 7
# ;

option show_stats 2;

printf "the_sum: %d\n", the_sum;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=rnd outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";


solve;

# display sums;

printf "s: ";
for{i in 1..n} {
  printf "%2d ", s[i];
}
printf "\n";
printf "x: ";
for{i in 1..n} {
  printf "%2d ", x[i];
}
printf "\n";

for{p in 1..num_subsets} {
  printf "Subset %2d: ", p;
  for{i in 1..n} {
   if x[i] = p then printf "%2d ", s[i];
  }
  printf "\n";
}
