/*
  Max flow, Winston OR, page 423

  Maximum flow
  Maxflow.lng

Lingo:
FLOW( 1, 2)        1.000000            0.000000
FLOW( 1, 3)        2.000000            0.000000
FLOW( 2, 3)        0.000000            0.000000
FLOW( 2, 4)        1.000000            0.000000
FLOW( 3, 5)        2.000000            0.000000
FLOW( 4, 5)        1.000000            0.000000
FLOW( 5, 1)        3.000000            0.000000

cplex:
FLOW :=
1 2   2
1 3   1
2 3   1
2 4   1
3 5   2
4 5   1
5 1   3
;

max flow (FLOW[5,1]) is the same as in the book: 3.

Note how ARCS is defined

set ARCS within (NODES cross NODES);

and the loops over the nodes:

   (j, i) in ARCS


Minos, lpsolve, bonmin gives the same result as in the book:

FLOW :=
1 2   1
1 3   2
2 3   0
2 4   1
3 5   2
4 5   1
5 1   3


and it works in glpk:

FLOW[5,1] = 3
FLOW[1,2] = 1
FLOW[1,3] = 2
FLOW[2,3] = 0
FLOW[2,4] = 1
FLOW[3,5] = 2
FLOW[4,5] = 1

(5,1) shows first since the sorting is on j instead of i

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/


set NODES;
set ARCS within (NODES cross NODES); # /1,2 1,3 2,3 2,4 3,5 4,5  5,1/ :CAP,FLOW;
param CAP{ARCS};

var FLOW {ARCS} >= 0;

maximize z:
        FLOW[5,1];


# @FOR(ARCS(I,J):FLOW(I,J)<CAP(I,J));
subject to c1{(i,j) in ARCS}:
        FLOW[i,j]<=CAP[i,j];
        
# @FOR(NODES(I):@SUM(ARCS(J,I):FLOW(J,I)) =@SUM(ARCS(I,J):FLOW(I,J)));
subject to c2{i in NODES}:
        sum{(j, i) in ARCS} FLOW[j,i] = sum{(i,j) in ARCS} FLOW[i,j];

data;

# 
set NODES := 1 2 3 4 5;

# CAP=2,3,3,4,2,1,1000;
param: ARCS: CAP := 
        1 2 2
        1 3 3 
        2 3 3
        2 4 4
        3 5 2
        4 5 1
        5 1 1000
;

# option solver cplex;
# option solver lpsolve;
# option solver bonmin;

solve;

display z;
display FLOW;

display _varname, _var, _var.slack, _var.lb, _var.ub;
display _conname, _con, _con.slack, _con.lb, _con.ub;
