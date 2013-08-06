/*

  Global constraint circuit in AMPL+CP.

  From Global Constraint Catalogue
  http://www.emn.fr/x-info/sdemasse/gccat/Ccircuit.html
  """
  Enforce to cover a digraph G described by the NODES collection with one 
  circuit visiting once all vertices of G.
  
  Example
      (
      <
      index-1	succ-2,​
      index-2	succ-3,​
      index-3	succ-4,​
      index-4	succ-1
      >
     )
  The circuit constraint holds since its NODES argument depicts the 
  following Hamiltonian circuit visiting successively the vertices 
  1, 2, 3, 4 and 1.
  """

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;

var x{1..n} >= 1 <= n integer;  # the successors
var z{1..n} >= 1 <= n integer;  # the path (orbits of x[1])

#
# constraints
#
s.t. c1: alldiff{i in 1..n} x[i];
s.t. c2: alldiff{i in 1..n} z[i];

s.t. c3: z[1] = x[1];

#
# put the orbit of x[1] in z[1..n]
# z[i] must not be 1 until i = n and then must be 1.
#
# MiniZinc code:
#   forall(i in lbx+1..ubx) (
#      z[i] = x[z[i-1]]
#   )
s.t. c4{i in 2..n}:
      exists{j in 1..n}
           z[i-1] = j and
           x[j] = z[i]
;

# may not be 1 for i < n
s.t. c5{i in 1..n-1}: z[i] != 1;

# when i = n it must be 1
s.t. c6: z[n] = 1;

# Testing
# s.t. c7: x[2] = n;

data;

param n := 25;

option solver gecode;
option gecode_options "var_branching=regret_min_max val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=2 debugexpr=0 logperiod=1 logverbosity=0";

solve;

printf "x: ";
for{i in 1..n} {
  printf "%2d ", x[i];
}
printf "\n";
printf "z: ";
for{i in 1..n} {
  printf "%2d ", z[i];
}
printf "\n";
