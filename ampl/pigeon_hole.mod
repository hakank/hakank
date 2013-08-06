/*

  Pigeon hole problem in AMPL+CP.

  From
  ftp://ftp.inria.fr/INRIA/Projects/contraintes/publications/CLP-FD/plilp94.html
  """
  pigeon: the pigeon-hole problem consists in putting n pigeons in m pigeon-holes (at most 1 pigeon per hole). The boolean formulation uses n × m variables to indicate, for each pigeon, its hole number. Obviously, there is a solution iff n <= m.
  """


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;
param m;


# decision variables
var x{1..n, 1..m} binary;

#
# constraints
#

# max 1 pigeon per pigeon hole
s.t. c1{j in 1..m}: sum{i in 1..n} x[i,j] <= 1;

# all pigeon must be placed and only at one hole
s.t. c2{i in 1..n}: sum{j in 1..m} x[i,j] = 1;

data;

param n := 2; # n pigeons
param m := 10; # m pigeon holes


option presolve 0;
# option show_stats 2;

option solver gecode;
option gecode_options "var_branching=degree_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";


solve;

for{i in 1..n} {
  for{j in 1..m} {
       printf "%d ", x[i,j];
  }
  printf "\n";
}
printf "\n";
