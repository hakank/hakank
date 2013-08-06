/*
  Winston OR, the example page 422: match making

  See max_flow_winston1.mod for comments

  Book (page 423):

   KC -> MS
   BR -> LA
   MJ -> VP
   TC -> KH

This model gets:

Kevin Costner -> Meryl Streep   <-
Tom Selleck -> Loni Anderson    <-
Michael Jackson -> Victoria Principal  <-
Tom Cruise -> Katherine Hepburn  <-

which must be correct as well.

MINOS och cbc give the same result as Winston:

Kevin Costner -> Meryl Streep: 1
Burt Reynolds -> Loni Anderson: 1
Michael Jackson -> Victoria Principal: 1
Tom Cruise -> Katherine Hepburn: 1

donlp2 yields (as usual) an interesting solution:

Kevin Costner -> Meryl Streep: 0.60
Burt Reynolds -> Loni Anderson: 0.60
Tom Selleck -> Loni Anderson: 0.40
Tom Selleck -> Meryl Streep: 0.40
Michael Jackson -> Victoria Principal: 1.00
Tom Cruise -> Katherine Hepburn: 0.50
Tom Cruise -> Linda Evans: 0.50

glpk:
Kevin Costner -> Meryl Streep: 1.00
Burt Reynolds -> Loni Anderson: 1.00
Michael Jackson -> Victoria Principal: 1.00
Tom Cruise -> Linda Evans: 1.00

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/


set PERSONS;
set ARCS within (PERSONS cross PERSONS);
param CAP{ARCS};
var FLOW {ARCS} >= 0;

maximize z:
        FLOW["sink", "source"];


# @FOR(ARCS(I,J):FLOW(I,J)<CAP(I,J));
# flödet får inte överstiga capaciteten
subject to c1{(i,j) in ARCS}:
        FLOW[i,j]<=CAP[i,j];
        
# @FOR(NODES(I):@SUM(ARCS(J,I):FLOW(J,I)) =@SUM(ARCS(I,J):FLOW(I,J)));
# summan av invärde = summan av utvärde
subject to c2{i in PERSONS}:
        sum{(j, i) in ARCS} FLOW[j,i] = sum{(i,j) in ARCS} FLOW[i,j];

data;

set PERSONS := "source" "Kevin Costner" "Burt Reynolds" "Tom Selleck" "Michael Jackson" "Tom Cruise" "Loni Anderson" "Meryl Streep" "Katherine Hepburn" "Linda Evans" "Victoria Principal" "sink";

param: ARCS: CAP := 
        "source" "Kevin Costner" 1
        "source" "Burt Reynolds" 1
        "source" "Tom Selleck" 1
        "source" "Michael Jackson" 1
        "source" "Tom Cruise" 1

        "Kevin Costner" "Meryl Streep" 1
        "Burt Reynolds" "Loni Anderson" 1
        # "Burt Reynolds" "Katherine Hepburn" 1  # a test
        "Tom Selleck" "Loni Anderson" 1
        "Tom Selleck" "Meryl Streep" 1
        "Michael Jackson" "Loni Anderson" 1
        "Michael Jackson" "Meryl Streep" 1
        "Michael Jackson" "Victoria Principal" 1
        "Tom Cruise" "Katherine Hepburn" 1
        "Tom Cruise" "Linda Evans" 1
        "Tom Cruise" "Victoria Principal" 1

        "Loni Anderson" "sink" 1
        "Meryl Streep" "sink" 1
        "Katherine Hepburn" "sink" 1
        "Linda Evans" "sink" 1
        "Victoria Principal" "sink" 1
        "sink" "source" 1000
        
;
      


option solver cplex;
# option solver lpsolve;
# option solver bonmin;
# option solver donlp2;
# option solver cbc;

solve;

display z;
display FLOW;

for{ (i,j) in ARCS: FLOW[i,j] > 0.1 and i not in {"sink", "source"} and j <> "sink"} {
        printf "%s -> %s: %0.2f\n", i, j, FLOW[i,j];
} 
printf "\n";
