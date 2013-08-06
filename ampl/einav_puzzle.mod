/*

  A programming puzzle from Einav in AMPL+CP.

  From 
  "A programming puzzle from Einav"
  http://gcanyon.wordpress.com/2009/10/28/a-programming-puzzle-from-einav/
  """
  My friend Einav gave me this programming puzzle to work on. Given this array of positive and negative numbers:
  33   30  -10 -6  18   7  -11 -23   6
  ...
  -25   4  16  30  33 -23  -4   4 -23

  You can flip the sign of entire rows and columns, as many of them
  as you like. The goal is to make all the rows and columns sum to positive
  numbers (or zero), and then to find the solution (there are more than one)
  that has the smallest overall sum. So for example, for this array:
  33  30 -10
  -16  19   9
  -17 -12 -14
  You could flip the sign for the bottom row to get this array:
  33  30 -10
  -16  19   9
  17  12  14
  Now all the rows and columns have positive sums, and the overall total is 
  108.
  But you could instead flip the second and third columns, and the second 
  row, to get this array:
  33  -30  10
  16   19    9
  -17   12   14
  All the rows and columns still total positive, and the overall sum is just 
  66. So this solution is better (I don’t know if it’s the best)
  A pure brute force solution would have to try over 30 billion solutions. 
  I wrote code to solve this in J. I’ll post that separately.
  """


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param rows;
param cols;
param datam{1..rows, 1..cols};

# decision variables
var x{1..rows, 1..cols} >= -100 <= 100 integer;

var row_sums{1..rows} >= 0 <= 200 integer;
var col_sums{1..cols} >= 0 <= 200 integer;

var row_signs{i in 1..rows} >= -1 <= 1 integer; # {-1,1}
var col_signs{j in 1..cols} >= -1 <= 1 integer; # {-1,1}

var total_sum >= 0 <= 1000 integer;

minimize obj: total_sum;

#
# constraints
#
s.t. c1{i in 1..rows, j in 1..cols}: x[i,j] = datam[i,j]*row_signs[i]*col_signs[j];

# total sum
s.t. c2: total_sum = sum{i in 1..rows, j in 1..cols} x[i,j];

# row sums
s.t. c3{i in 1..rows}: row_sums[i] = sum{j in 1..cols} (row_signs[i]*col_signs[j]*datam[i,j]);

# column sums
s.t. c4{j in 1..cols}: col_sums[j] = sum{i in 1..rows} (row_signs[i]*col_signs[j]*datam[i,j]);

s.t. c5{i in 1..rows}: row_signs[i] != 0;
s.t. c6{j in 1..cols}: col_signs[j] != 0;

data;


param rows := 27;
param cols := 9;
param datam: 1 2 3 4 5 6 7 8 9 := 
 1  33 30 10 -6 18 -7 -11 23 -6 
 2  16 -19 9 -26 -8 -19 -8 -21 -14 
 3  17 12 -14 31 -30 13 -13 19 16 
 4  -6 -11 1 17 -12 -4 -7 14 -21 
 5  18 -31 34 -22 17 -19 20 24 6 
 6  33 -18 17 -15 31 -5 3 27 -3 
 7  -18 -20 -18 31 6 4 -2 -12 24 
 8  27 14 4 -29 -3 5 -29 8 -12 
 9  -15 -7 -23 23 -9 -8 6 8 -12 
10  33 -23 -19 -4 -8 -7 11 -12 31 
11  -20 19 -15 -30 11 32 7 14 -5 
12  -23 18 -32 -2 -31 -7 8 24 16 
13  32 -4 -10 -14 -6 -1 0 23 23 
14  25 0 -23 22 12 28 -27 15 4 
15  -30 -13 -16 -3 -3 -32 -3 27 -31 
16  22 1 26 4 -2 -13 26 17 14 
17  -9 -18 3 -20 -27 -32 -11 27 13 
18  -17 33 -7 19 -32 13 -31 -2 -24 
19  -31 27 -31 -29 15 2 29 -15 33 
20  -18 -23 15 28 0 30 -4 12 -32 
21  -3 34 27 -25 -18 26 1 34 26 
22  -21 -31 -10 -13 -30 -17 -12 -26 31 
23  23 -31 -19 21 -17 -10 2 -23 23 
24  -3 6 0 -3 -32 0 -10 -25 14 
25  -19 9 14 -27 20 15 -5 -27 18 
26  11 -6 24 7 -17 26 20 -31 -25 
27  -25 4 -16 30 33 23 -4 -4 23
;

# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=degree_max val_branching=max outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=auto alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display total_sum;
display row_sums, row_signs;
display col_sums, col_signs;
display x;



