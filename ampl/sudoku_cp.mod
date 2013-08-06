/*

  Sudoku solver in AMPL+CP.

  See 
     http://en.wikipedia.org/wiki/Sudoku

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;
param reg := ceil(sqrt(n));
param m{1..n, 1..n} default 0;

var x{1..n, 1..n} >= 1 <= n integer;


#
# constraints
#

# Fill the hints
s.t. c1{i in 1..n, j in 1..n: m[i,j] > 0}: x[i,j] = m[i,j]; 

# Latin square
s.t. c2{i in 1..n}: alldiff{j in 1..n} x[i,j]; # rows
s.t. c3{j in 1..n}: alldiff{i in 1..n} x[i,j]; # columns

# Regions
s.t. c4{i in 0..reg-1, j in 0..reg-1}: 
     alldiff{r in i*reg+1..i*reg+reg, c in j*reg+1..j*reg+reg} x[r,c];

# data sudokuVeryEasy.dat;
data sudokuHard.dat;

# data;

# http://www.ampl.com/NEW/LOGIC/EXAMPLES/sudokuVeryEasy.dat
# param n := 9;
# param m: 1 2 3 4 5 6 7 8 9 :=
#    1   . . 1 . 8 . 3 . .
#    2   4 5 . . 6 . . 2 .
#    3   . . 9 . . 3 7 6 .
#    4   . . . 8 9 . . 3 7
#    5   . 1 7 5 . 2 6 9 .
#    6   3 9 . . 4 7 . . .
#    7   . 4 5 9 . . 8 . .
#    8   . 2 . . 5 . . 1 6
#    9   . . 6 . 7 . 4 . . 
# ;


# http://www.ampl.com/NEW/LOGIC/EXAMPLES/sudokuHard.dat
# param n := 9;
# param m: 1 2 3 4 5 6 7 8 9 :=
#       1   2 . 4 . . . . 5 .
#       2   . 9 . . . 8 . . .
#       3   8 . . 1 . . . 2 .
#       4   . . 7 3 6 . . 8 .
#       5   . . 8 . . . 2 . .
#       6   . 3 . . 2 1 7 . .
#       7   . 8 . . . 3 . . 4
#       8   . . . 5 . . . 6 .
#       9   . 4 . . . . 8 . 5 
# ;


option solver gecode;
option gecode_options "icl=dom var_branching=regret_min_max val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=1 logverbosity=0";

solve;

printf "x:\n";
for{i in 1..n} {
  for{j in 1..n} {
     printf "%2d ", x[i,j];
  }
  printf "\n";
}

printf "\n";
