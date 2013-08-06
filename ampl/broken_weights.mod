/*

  Broken weights problem in AMPL+CP.

  From
  http://www.mathlesstraveled.com/?p=701
  """
  Here's a fantastic problem I recently heard. Apparently it was first 
  posed by Claude Gaspard Bachet de Méziriac in a book of arithmetic problems 
  published in 1612, and can also be found in Heinrich Dorrie’s 100 
  Great Problems of Elementary Mathematics.
  
      A merchant had a forty pound measuring weight that broke 
      into four pieces as the result of a fall. When the pieces were 
      subsequently weighed, it was found that the weight of each piece 
      was a whole number of pounds and that the four pieces could be 
      used to weigh every integral weight between 1 and 40 pounds. What 
      were the weights of the pieces?
  
  Note that since this was a 17th-century merchant, he of course used a 
  balance scale to weigh things. So, for example, he could use a 1-pound 
  weight and a 4-pound weight to weigh a 3-pound object, by placing the 
  3-pound object and 1-pound weight on one side of the scale, and 
  the 4-pound weight on the other side.
  """

  Also, compare with the coin change problem:
    http://www.hakank.org/ampl/coins3.mod


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n; # number of different weights
param m; # original weight

var weights{1..n} >= 1 <= m integer; # the weights
var x{1..m, 1..n} >= -1 <= 1 integer; # the combinations

minimize obj: weights[n];

#
# constraints
#
s.t. c1{i in 2..n}: weights[i-1] <= weights[i];

s.t. c2: m = sum{i in 1..n} weights[i];

# Check that all weights from 1 to 40 can be made.
#  
# Since all weights can be on either side
# of the side of the scale we allow either
# -1, 0, or 1 or the weights, assuming that
# -1 is the weights on the left and 1 is on the right.
# 
s.t. c3{j in 1..m}:
      sum{i in 1..n} (x[j,i]*weights[i]) = j 
;

data;

param n := 4;
param m := 40;

option solver gecode;
option gecode_options "var_branching=degree_max val_branching=rnd outlev=1 outfreq=1 timelimit=30";
# option solver ilogcp;

solve;

#display weights;
printf "    ";
for{i in 1..n} {
  printf "%2d ", weights[i];
}
printf "\n";
for{j in 1..m} {
  printf "%2d: ", j;
  for{i in 1..n} {
     printf "%2d ", x[j,i];
  }
  printf "\n";
}
printf "\n";
