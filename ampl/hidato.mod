/*

  Hidato puzzle in AMPL+CP.

  http://www.shockwave.com/gamelanding/hidato.jsp
  http://www.hidato.com/

  """
  Puzzles start semi-filled with numbered tiles.
  The first and last numbers are circled.
  Connect the numbers together to win. Consecutive
  number must touch horizontally, vertically, or
  diagonally.
  """  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param puzzle{1..n, 1..n} default 0;

var x{1..n, 1..n} >= 1 <= n*n  integer;

#
# constraints
#
s.t. c1: alldiff{i in 1..n, j in 1..n} x[i,j];

# Place the integers 1..n*n
s.t. c2{i in 1..n, j in 1..n: puzzle[i,j] > 0}:
         x[i,j] = puzzle[i,j];

# This is extremely slow...
s.t. c3{k in 1..n*n-1}:
     exists{i in 1..n, j in 1..n} (
        k = x[i, j] # fix this k
        and
        exists{a in {-1, 0, 1}, b in {-1, 0, 1}:
          i+a >= 1 and j+b >=  1 and
          i+a <= n and j+b <= n and
          !(a = 0 and b = 0) 
          # /\ abs(a) + abs(b) >= 1 
        }
        # find the next k
        (k + 1 = x[i+a, j+b])
     )
;

# data hidato1.dat;
# data hidato2.dat;
# data hidato3.dat;
# data hidato4.dat;
# data hidato5.dat;
data hidato6.dat;
# data hidato7.dat;
# data hidato8.dat;

# data;

## This is the same as hidato1.dat
## solution:
##  6 7 9
##  5 2 8
##  1 4 3
# param n := 3;
# param puzzle: 1 2 3  := 
#   1   6 . 9 
#   2   . 2 8 
#   3   1 . .
# ;


option presolve 0;

option solver gecode;
option gecode_options "var_branching=size_max val_branching=min outlev=1 outfreq=1";
# option solver ilogcp;

solve;

#write gtest;

for{i in 1..n} {
  for{j in 1..n} {
        printf "%2d ", x[i,j];
  }
  printf "\n";
}
printf "\n";
