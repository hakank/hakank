/*

  Knight domination in AMPL.

  Minimize the number of knights needed to cover all squares on 
  a n x n chessboard.


  n   minnum   time(cplex 8 cores available)
 -------------------------

  1      -
  2      -
  3      -
  4      6      0.017s
  5      7      0.037s
  6      8      0.035s
  7     10      0.021s
  8     14      0.022s
  9     18      0.065s
 10     22      0.059s
 11     25      0.099s
 12     28      0.280s
 13     32      0.207s
 14     36      0.258s
 15     43      0.754s
 16     48      2.880s
 17     54      4.081s
 18     58      5.128s
 19     -       -

 This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
 See also my AMPL page: http://www.hakank.org/ampl/


*/

param n = 8;

var x{1..n, 1..n} binary;

minimize minnum: 
  sum{i in 1..n,j in 1..n} x[i,j];

# Every square is threatened
subject to setzero{i in 1..n, j in 1..n}:
         sum{a in {-2,-1,1,2}, b in {-2,-1,1,2}: a+i > 0 and a+i <= n and
               b+j > 0 and b+j <= n and
               abs(a)+abs(b) = 3 } x[i+a,j+b] >= 1;

option solver cplex;
# option solver cbc;
solve;

display minnum;

# display x;

for{i in 1..n} {
    for{j in 1..n} {
      printf "%d  ",x[i,j];
    }
    printf "\n";
}
