/*

were4 from Chlond's artikel

IP Modeling and the Logical Puzzles of Raymond Smullyan
http://ite.pubs.informs.org/Vol3No3/ChlondToase/
 
Code:
http://ite.pubs.informs.org/Vol3No3/ChlondToase/were4.php

model 'were4'

! Description  : Werewolves IV
! Source       : Smullyan, R., (1978), What is the Name of this Book?, Prentice-Hall
! Date written : 20/12/99
! Written by   : M J Chlond 

Output:
CPLEX 10.1.0: optimal integer solution; objective 1
0 MIP simplex iterations
0 branch-and-bound nodes
:   x   y    :=
1   1   1
2   0   0
3   0   0
;


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

param person = 3;
var x{1..person} binary; # x(i) = 1 if person i is a knight, 0 if a knave
var y{1..person} binary; # y(i) = 1 if person i is a werewolf, 0 otherwise

minimize any: 
        x[1];
  
# if statement 1 is true then x[1) = 1, else 0
subject to lca1:
        sum{i in 1..person} x[i]+3*x[1] >= 3;

        lca2: sum{i in 1..person} x[i]+3*x[1] <= 5;

# if statement 2 is true then x[2) = 1, else 0
        lcb: x[3] = x[2];

# only one is a werewolf
        pca: sum{i in 1..person} y[i] = 1;

# werewolf is a knight
        pcb{i in 1..person}: x[i] >= y[i];


option solver cplex;
# option solver bonmin;
solve;

display x,y;