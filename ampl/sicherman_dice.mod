/*

  Sicherman Dice in AMPL+CP.

  From http://en.wikipedia.org/wiki/Sicherman_dice
  """ 
  Sicherman dice are the only pair of 6-sided dice which are not normal dice, 
  bear only positive integers, and have the same probability distribution for 
  the sum as normal dice.
  
  The faces on the dice are numbered 1, 2, 2, 3, 3, 4 and 1, 3, 4, 5, 6, 8.
  """

  I read about this problem in a book/column by Martin Gardner long
  time ago, and got inspired to model it now by the WolframBlog post
  "Sicherman Dice": http://blog.wolfram.com/2010/07/13/sicherman-dice/

  This model gets the two different ways, first the standard way and
  then the Sicherman dice:
  
  x1 = [1, 2, 3, 4, 5, 6]
  x2 = [1, 2, 3, 4, 5, 6]
  ----------
  x1 = [1, 2, 2, 3, 3, 4]
  x2 = [1, 3, 4, 5, 6, 8]


  Extra: If we also allow 0 (zero) as a valid value then the 
  following two solutions are also valid:
  
  x1 = [0, 1, 1, 2, 2, 3]
  x2 = [2, 4, 5, 6, 7, 9]
  ----------
  x1 = [0, 1, 2, 3, 4, 5]
  x2 = [2, 3, 4, 5, 6, 7]
  
  These two extra cases are mentioned here:
  http://mathworld.wolfram.com/SichermanDice.html


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;
param m; # max integer

param standard_dist{2..12};

var x1{1..n} >= 1 <= m integer; 
var x2{1..n} >= 1 <= m integer; 

#
# constraints
#
s.t. c1{k in 2..12}: 
     standard_dist[k] = count {i in 1..n, j in 1..n} ( x1[i]+x2[j] == k);

# Symmetry breaking
s.t. c2{i in 2..n}: x1[i-1] <= x1[i];
s.t. c3{i in 2..n}: x2[i-1] <= x2[i];

s.t. c4{i in 1..n}: x1[i] <= x2[i];

# To test the model we require that the two dice are not identical.
s.t. c5: exists{i in 1..n} x1[i] != x2[i];

data;

param n := 6;
param m := 10;

param standard_dist := 
  2  1
  3  2
  4  3 
  5  4 
  6  5 
  7  6 
  8  5 
  9  4 
 10  3 
 11  2 
 12  1
;  

option solver gecode;
option gecode_options "var_branching=size_min val_branching=med outlev=1 outfreq=1 timelimit=30";
# option solver ilogcp;

solve;

#display weights;
printf "x1: ";
for{i in 1..n} {
  printf "%2d ", x1[i];
}
printf "\nx2: ";
for{i in 1..n} {
  printf "%2d ", x2[i];
}
printf "\n";
