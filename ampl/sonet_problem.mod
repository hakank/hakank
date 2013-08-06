/*

  SONET problem in AMPL+CP.

  Translation of the ESSENCE' model in the Minion Translator examples:
  http://www.cs.st-andrews.ac.uk/~andrea/examples/sonet/sonet_problem.eprime
  """
  The SONET problem is a network design problem: set up a network between
  n nodes, where only certain nodes require a connection.
  Nodes are connected by putting them on a ring, where all nodes
  on a ring can communicate. Putting a node on a ring requires a so-called
  ADM, and each ring has a capacity of nodes, i.e. ADMs. There is a certain 
  amount of rings, r, that is available. The objective is to set up a network
  by using a minimal amount of ADMs.


  About the problem model

  The problem model has the amount of rings ('r'), amount of nodes('n'),
  the 'demand' (which nodes require communication) and node-capacity of each 
  ring ('capacity_nodes') as parameters.
  The assignement of nodes to rings is modelled by a 2-dimensional matrix 'rings',
  indexed by the amnount of rings and nodes. The matrix-domain is boolean:
  If the node in column j is assigned to the ring in row i, then rings[i,j] = 1 
  and 0 otherwise. So all the '1's in the matrix 'rings' stand for an ADM.
  Hence the objective is to minimise the sum over all columns and rows of matrix
  'rings'.
  """


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param r; # upper bound for amount of rings
param n; # number of clients
param demand{1..n, 1..n}; 
param capacity_nodes{1..r}; 

# decision variables
var rings{1..r, 1..n} binary;
var z >= 0 integer;

minimize obj: z;

#
# constraints
#

# "if there is a demand between 2 nodes, then there has to exist 
#  a ring, on which they are both installed"
# 
s.t. c1{client1 in 1..n, client2 in 1..n: client1 < client2}:
   (demand[client1,client2] = 1) ==>
        exists{ring in 1..r}
              rings[ring,client1] + rings[ring, client2] >= 2
;

# capacity of each ring must not be exceeded     
s.t. c2{ring in 1..r}:
     sum{client in 1..n} rings[ring, client] <= capacity_nodes[ring] 
;


s.t. c3: z = sum{ring in 1..r, client in 1..n} rings[ring, client];

data;

param r := 4;
param n := 5;

param demand: 1 2 3 4 5 :=
  1 0 1 0 1 0 
  2 1 0 1 0 0 
  3 0 1 0 0 1 
  4 1 0 0 0 0 
  5 0 0 1 0 0
;

param capacity_nodes := 
  1 3
  2 2 
  3 2 
  4 1
;



option show_stats 2;
# option presolve 0;

option solver gecode;
option gecode_options "var_branching=size_max val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;


display z;
# display rings;
for{i in 1..r} {
  for{j in 1..n} {
    printf "%d ", rings[i,j];
  }
  printf "\n";
}
printf "\n";
