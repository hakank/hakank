/*
  Tue Jan  1 11:36:29 2008/hakank@bonetmail.com

  Winston OR, 453: Minimum-Cost Network Flow Problem (MCNFP)
  Translation of the Lingo model Traffic.lng

Lingo gives:
FLOW( 1, 2)        700.0000            0.000000
FLOW( 1, 3)        200.0000            0.000000
FLOW( 2, 4)        600.0000            0.000000
FLOW( 2, 5)        100.0000            0.000000
FLOW( 3, 4)        200.0000            0.000000
FLOW( 3, 5)        0.000000            20.00000
FLOW( 4, 5)        400.0000            0.000000
FLOW( 4, 6)        400.0000            0.000000
FLOW( 5, 6)        500.0000            0.000000

MINOS:
z = 95000

FLOW :=
1 2   700
1 3   200
2 4   600
2 5   100
3 4   200
3 5     0
4 5   400
4 6   400
5 6   500
;

Which is the same.

CPLEX and bonmin give another solution:
z = 95000

FLOW :=
1 2   700
1 3   200
2 4   600
2 5   100
3 4   200
3 5     0
4 5   500   <-
4 6   300   <-
5 6   600   <-
;


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

set NODES;
param SUPP{NODES};

set ARCS within {NODES cross NODES};
param CAP{ARCS} >= 0;
param COST{ARCS} >= 0;
var FLOW{ARCS} >= 0;

# MIN=@SUM(ARCS:COST*FLOW);
minimize z:
        sum{(i,j) in ARCS} COST[i,j]*FLOW[i,j];

# @FOR(ARCS(I,J):FLOW(I,J)<CAP(I,J));
subject to c1{ (i,j) in ARCS}:
        FLOW[i,j] <= CAP[i,j];

# @FOR(NODES(I):-@SUM(ARCS(J,I):FLOW(J,I)) +@SUM(ARCS(I,J):FLOW(I,J))=SUPP(I));
# notera att det är _minus_ av första summan (vilket jag först missade)
subject to c2{i in NODES}:
        -sum{ (j, i) in ARCS} FLOW[j,i] + sum{(i,j) in ARCS} FLOW[i,j] = SUPP[i];


data;

set NODES:= 1 2 3 4 5 6;

# COST=10,50,30,70,10,60,30,60,30;
# CAP=800,600,600,100,300,400,600,400,600;
param: ARCS: COST CAP := 
        1,2  10 800
        1,3  50 600
        2,4  30 600
        2,5  70 100
        3,4  10 300
        3,5  60 400
        4,5  30 600
        4,6  60 400
        5,6  30 600
;
 

param SUPP :=
        1 900,
        2 0,
        3 0,
        4 0,
        5 0,
        6 -900
;

#option cplex_option "sensitivity";
 option solver cplex;
# option solver bonmin;
#option solver lpsolve;

solve;
display z;
display FLOW;
for{ (i,j) in ARCS} {
        printf "%d %d: %d\n", i,j, FLOW[i,j];
}