/*

  Golomb ruler problem (CSPLib #6) in AMPL+CP.

  Golomb rulers
  """
  A Golomb ruler may be defined as a set of m integers 0 = a_1 < a_2 <
  ... < a_m such that the m(m-1)/2 differences a_j - a_i, 1 <= i < j
  <= m are distinct. Such a ruler is said to contain m marks and is of
  length a_m. The objective is to find optimal (minimum length) or
  near optimal rulers.
  """

  Also see 
     http://en.wikipedia.org/wiki/Golomb_ruler

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n_marks;
param maxval = n_marks^2;
param numOfDifferences = (n_marks*(n_marks-1)/2);

# decision variables
var ruler{1..n_marks} >= 0 <= maxval integer;
var distance{1..numOfDifferences} >= 1 <= maxval integer;

minimize obj: ruler[n_marks];

#
# constraints
#
s.t. c1: ruler[1] = 0;
s.t. c2{m1 in 1..n_marks, m2 in 1..n_marks: m2 > m1}:
    distance[((n_marks*(n_marks-1)/2) - ((n_marks-m1+1)*(n_marks-m1)/2) + (m2-m1))] = ruler[m2] - ruler[m1]
;
s.t. c3: alldiff{i in 1..numOfDifferences} distance[i];
# increasing
s.t. c4{i in 1..n_marks-1}: ruler[i] < ruler[i+1];

s.t. c5: (ruler[2] - ruler[1]) < (ruler[n_marks] - ruler[n_marks - 1]);


data;

param n_marks := 10;


# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "icl=bnd var_branching=size_max val_branching=min outlev=1 outfreq=1";


# option solver ilogcp;
# option ilogcp_options "optimizer=auto alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display ruler;
display distance;



