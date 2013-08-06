/*
   Taha OR, page 378, excersise 7
   About "bisarr behaviour" with Branch & Bound.

   AMPL handle the problem with presolve, but I check
   what's happening when skipping presolve.

With presolve:
CPLEX 10.1.0: optimal integer solution; objective 1
0 MIP simplex iterations
0 branch-and-bound nodes
x [*] :=
 1  1
 2  1
 3  1
 4  1
 5  1
 6  1
 7  1
 8  0
 9  0
10  0
11  0
12  0
13  0
14  0
15  0
;

y = 1
z = 1


With presolve:
With cplex there is not difference at all: Still 0 MIP and 0 b&b nodes.


bonmin (with presolve):
bonmin: Optimal
x [*] :=
 1  0
 2  0
 3  0
 4  0
 5  0
 6  0
 7  0
 8  0
 9  1
10  1
11  1
12  1
13  1
14  1
15  1
;

y = 1
z = 1


lpsolve:
LP_SOLVE 5.5.0.10: optimal, objective 1
19311 simplex iterations
25738 branch & bound nodes: depth 16
x [*] :=
 1  1
 2  1
 3  1
 4  1
 5  1
 6  1
 7  0
 8  1
 9  0
10  0
11  0
12  0
13  0
14  0
15  0
;

y = 1
z = 1

Here are the 25000 b&b nodes


glpk:
x[1] = 1
x[2] = 0
x[3] = 0
x[4] = 0
x[5] = 0
x[6] = 0
x[7] = 0
x[8] = 0
x[9] = 1
x[10] = 1
x[11] = 1
x[12] = 1
x[13] = 1
x[14] = 1
x[15] = 0
y = 1
z:
   1 y



This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

param n := 15;
var x{1..n} binary;
var y binary;
# var y integer; # just playing

minimize z: y;

# The problem is simply to select 7 variables
# (it's much less work if convering this to a knapsack problem,
# i.e. multiplies with x[i])
#s.t. c1: y + 2*sum{i in 1..n} i*x[i] = 15; # the knapsack variant
s.t. c1: y + 2*sum{i in 1..n} x[i] = 15;

#option presolve 0;
# option solver lpsolve;
#option solver bonmin;
option solver cplex;
solve;

display x,y,z;

