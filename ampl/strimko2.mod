/*

  Strimko problem in AMPL+CP.

  From 
  360: A New Twist on Latin Squares
  http://threesixty360.wordpress.com/2009/08/04/a-new-twist-on-latin-squares/
  """
  The idea is simple: each row and column of an nxn grid must contain 
  the number 1, 2, ... n exactly once (that is, the grid must form a 
  Latin square), and each "stream" (connected path in the grid) must 
  also contain the numbers 1, 2, ..., n exactly once.
  """

  For more information, see:
  * http://www.strimko.com/
  * http://www.strimko.com/rules.htm
  * http://www.strimko.com/about.htm
  * http://www.puzzlersparadise.com/Strimko.htm
  


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param streams{1..n, 1..n};
param num_placed;
param placed{1..num_placed, 1..3};

var x{1..n, 1..n} >= 1 <= n integer;

#
# constraints
#
# latin square
s.t. c1a{i in 1..n}: alldiff{j in 1..n} x[i, j];
s.t. c1b{i in 1..n}: alldiff{j in 1..n} x[j, i];


# streams
s.t. c2{s in 1..n}:
    alldiff{i in 1..n, j in 1..n: streams[i,j] = s} x[i,j]
;

# placed
s.t. c3{i in 1..num_placed}:
       x[placed[i,1], placed[i,2]] = placed[i,3]
;

# data strimko2_002.dat;
# data strimko2_067.dat;
# data strimko2_068.dat;
# data strimko2_069.dat;
# data strimko2_070.dat;
data strimko2_conceptis_069.dat;


# data;

# # Strimko Set 068
# param n := 4;

# # represent the different streams with integer 1..n
# param streams: 1 2 3 4 :=
#  1    1 2 2 4 
#  2    2 1 4 2 
#  3    3 4 1 3 
#  4    4 3 3 1
# ;

# param num_placed := 3;
# param placed: 1 2 3 :=
#   1  2 2 3 
#   2  2 3 2 
#   3  3 3 1
# ;


option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

for{i in 1..n} {
  for{j in 1..n} {
    printf "%d ", x[i,j];
  }
  printf "\n";
}
printf "\n";

