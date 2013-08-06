/*

  Talisman Square in AMPL+CP.

  http://mathworld.wolfram.com/TalismanSquare.html
  """
  An n×n array  of the integers from 1 to n^2 such that the difference between 
  any one integer and its neighbor (horizontally, vertically, or diagonally, without 
  wrapping around) is greater than or equal to some value k is called a (n,k)-talisman 
  square. 
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param k;

# decision variables
var x{1..n, 1..n} >= 1 <= n*n integer;

#
# constraints
#
s.t. c1: alldiff{i in 1..n, j in 1..n} x[i,j];

s.t. c2{i in 2..n, j in 2..n}:
  abs(x[i,j]-x[i-1,j]) >= k and
  abs(x[i,j]-x[i,j-1]) >= k
;

s.t. c3{i in 1..n-1, j in 1..n-1}:
  abs(x[i,j]-x[i+1,j]) >= k and
  abs(x[i,j]-x[i,j+1]) >= k
;

# some symmetry breaking
# s.t. c4: x[1,1] = 1;


data;

param n := 5;
param k := 2;

option show_stats 2;
# option presolve 0;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=med outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

for{i in 1..n} {
  for{j in 1..n} {
    printf "%2d ", x[i,j];
  }
  printf "\n";
}

printf "\n";