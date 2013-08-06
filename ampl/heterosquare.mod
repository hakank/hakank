/*

  Heterosquare problem in AMPL+CP.

  From http://willow.engr.uconn.edu/cometPubWiki/index.php/Heterosquare
  """
  A heterosquare of order n is a n*n square whose elements are distinct integers from 
  1 to n^2 such that the sums of the rows, columns and diagonals are all different. 
  Here is an example of heterosquare of order 3 
  
             19
  
  1  2  3    6
  8  9  4    21
  7  6  5    18
  
  16 17 12   15  (Sums)
  """


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

# decision variables
var mat{1..n,1..n} >= 1 <= n*n integer;
var row_sums{1..n} >= 1 <= n*n*n integer;
var col_sums{1..n} >= 1 <= n*n*n integer;
var diag1 >= 1 <= n*n*n integer;
var diag2 >= 1 <= n*n*n integer;

# For all sums
var allsums{1..2*n+2} >= 1 <= n*n*n integer;

#
# constraints
#
# all the entries in the matrix should be different
s.t. c1: alldiff{i in 1..n, j in 1..n} mat[i,j];

# all sums should be different
#     allsums = row_sums ++ col_sums ++ [diag1, diag2]
s.t. c2a{i in 1..n}:
    allsums[i]   = row_sums[i] and
    allsums[i+n] = col_sums[i]
;

s.t. c2b: 
     allsums[2*n+1] = diag1 
     and
     allsums[2*n+2] = diag2
 ;

s.t. c2d: alldiff{i in 1..2*n+2} allsums[i]; 

# rows sums
s.t. c3{i in 1..n}: sum{j in 1..n} mat[i,j] = row_sums[i];

# column sums
s.t. c4{j in 1..n}: sum{i in 1..n} mat[i,j] = col_sums[j];

# diag1 sums
s.t. c5: sum{i in 1..n} mat[i,i] = diag1;

# diag2 sums
s.t. c6: sum{i in 1..n} mat[i,n-i+1] = diag2;

#
# symmetry breaking
#
# From http://en.wikipedia.org/wiki/Heterosquare
# """
# It is strongly suspected that there are exactly 3120 
# essentially different heterosquares of order 3.
# """
#
# From
# http://en.wikipedia.org/wiki/Fr#C3#A9nicle_standard_form
# """
# A magic square is in Frénicle standard form, named for 
# Bernard Frénicle de Bessy, if the following two conditions apply:
#  - the element at position [1,1] (top left corner) is the smallest 
#    of the four corner elements; and
#  - the element at position [1,2] (top edge, second from left) is 
#    smaller than the element in [2,1].
# """
# (Note: For n=3 this gives 3120 solutions, as suspected...)
#
s.t. c7:
  mat[1,1] < mat[1,n] and
  mat[1,1] < mat[n,1] and
  mat[1,1] < mat[n,n] and
  mat[1,2] < mat[2,1]
;


data;

param n := 3;


# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1";


# option solver ilogcp;
# option ilogcp_options "optimizer=auto alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

# display mat;
printf "mat:\n";
for{i in 1..n} {
  for{j in 1..n} {
     printf "%2d ", mat[i,j];
  }
  printf "\n";
}
printf "\n";
printf "mat: ";
for{i in 1..n} {
  for{j in 1..n} {
     printf "%d, ", mat[i,j];
  }
}
printf "\n";


display row_sums;
display col_sums;
display diag1, diag2;
display allsums;


