/*

  Latin square card puzzle in AMPL+CP.

  Problem from Mario Livio's book about group theory
  "The Equation that couldn't be solved",
  page 22
  """
  "... Incidentally, you may get a kick out of solving this
  eighteenth century card puzzle: Arrange all the jacks,
  queens, kings, and aces from a deck of cards in a square so that 
  no suit or value would appear twice in any row, column, or the
  two main diagonals.
  """
  
  Also see
  - http://en.wikipedia.org/wiki/Graeco-Latin_square
  - http://en.wikipedia.org/wiki/Thirty-six_officers_problem


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param m;

set cards = setof{i in 0..n-1, j in 0..n-1} (i+m*j);

var x{1..n, 1..n} >= 0 <= m*n-1 integer;


#
# constraints
#

# use only the valid values
# (this don't do what I expect...)
# s.t. c0{i in 1..n, j in 1..n}: x[i,j] in cards;

s.t. c0{i in 1..n, j in 1..n}: 
    x[i,j] mod m < n
;

# all values must be different 
s.t. c1: alldiff{i in 1..n, j in 1..n} x[i,j];

# diagonals1
s.t. c2a: alldiff{i in 1..n} x[i,i] mod m;
s.t. c2b: alldiff{i in 1..n} x[i,i] div m;

# diagonal2
s.t. c3a: alldiff{i in 1..n} x[i,n-i+1] mod m;
s.t. c3b: alldiff{i in 1..n} x[i,n-i+1] div m;

# rows, columns, 
s.t. c4a{i in 1..n}: alldiff{j in 1..n} x[i,j] div m;
s.t. c4b{i in 1..n}: alldiff{j in 1..n} x[j,i] div m;

s.t. c4c{i in 1..n}: alldiff{j in 1..n} x[i,j] mod m;
s.t. c4d{i in 1..n}: alldiff{j in 1..n} x[j,i] mod m;


# symmetry breaking
s.t. c5: x[1,1] = 0;


data;

param n := 4;
param m := 10;

# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display cards;

for{i in 1..n} {
  for{j in 1..n} {
    printf "%2d ", x[i,j];
  }
  printf "\n";
}
for{i in 1..n} {
  for{j in 1..n} {
    printf "%d, ", x[i,j];
  }
}
printf "\n";