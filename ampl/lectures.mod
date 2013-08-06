/*

  Lectures problem in AMPL+CP.

  Biggs: Discrete Mathematics (2nd ed), page 187.
  """
  Suppose we wish to schedule six one-hour lectures, v1, v2, v3, v4, v5, v6.
  Among the the potential audience there are people who wish to hear both

   - v1 and v2
   - v1 and v4
   - v3 and v5
   - v2 and v6
   - v4 and v5
   - v5 and v6
   - v1 and v6

  How many hours are necessary in order that the lectures can be given
  without clashes?
  """

  Note: We can see this as a coloring problem.

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;     # number of nodes
param edges; # number of edges
set nodes = 1..n;
param g{1..edges, 1..2};

var v{nodes} >= 1 <= n integer;
var max_c >= 1 <= n integer;

minimize obj: max_c;

#
# constraints
#
s.t. c1{i in 1..edges}: v[g[i,1]] != v[g[i,2]];

s.t. c2: max_c = max{i in nodes} v[i];

# symmetry breaking: v1 has the "color" 1, v2 has either "color" 1 or 2
# (this should be general enough for a general model)
s.t. c3: v[1] = 1 and v[2] <= 2;

data;

param n := 6; 
param edges := 7;

# The schedule requirements:
#    lecture a cannot be held at the same time 
#    (i.e. has the same "color") as b.
#
param g: 1 2 :=
 1  1 2
 2  1 4
 3  3 5
 4  2 6
 5  4 5
 6  5 6
 7  1 6
;



option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display v;
display max_c;
