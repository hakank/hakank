/*

  Global constraint global_contiguity in AMPL+CP.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_contiguity.html
  """
  Enforce all variables of the VARIABLES collection to be assigned to 0 or 1. 
  In addition, all variables assigned to value 1 appear contiguously.
  
  Example:
  (<0,​1,​1,​0>)
  
  The global_contiguity constraint holds since the sequence 0 1 1 0 contains 
  no more than one group of contiguous 1.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

# decision variables
var x{1..n} binary;
var y{1..n} >= 0 <= 2 integer;

#
# constraints
#
s.t. c1{i in 2..n}: y[i-1] <= y[i];
s.t. c2{i in 1..n}: 
   (x[i] = 1) <==> (y[i] = 1)
;

# make it more interesting: there must be atleast three 1s.
s.t. c3:  3 = count{i in 1..n} (x[i] = 1);

data;


param n := 10;


option show_stats 2;
# option presolve 0;

option solver gecode;
option gecode_options "var_branching=regret_min_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=auto alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display x;
