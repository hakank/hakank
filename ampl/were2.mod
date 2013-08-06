/*
  were2 from Chlond's article

IP Modeling and the Logical Puzzles of Raymond Smullyan
http://ite.pubs.informs.org/Vol3No3/ChlondToase/
 
Code:
http://ite.pubs.informs.org/Vol3No3/ChlondToase/were2.php


model 'were2'

! Description  : Werewolves II
! Source       : Smullyan, R., (1978), What is the Name of this Book?, Prentice-Hall
! Date written : 20/12/99
! Written by   : M J Chlond 

  uses 'mmxprs'
  
Output:

CPLEX 10.1.0: optimal integer solution; objective 0
0 MIP simplex iterations
0 branch-and-bound nodes
:   x   y    :=
1   0   0
2   0   0
3   1   1
;

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

param person = 3;

var x{1..person} binary; # x(i) = 1 if person i is a knight, 0 if a knave
var y{1..person} binary; # y(i) = 1 if person i is a werewolf, 0 otherwise

minimize any:
        x[1];

#  ! only one is a werewolf
subject to pca: sum{i in 1..person} y[i] = 1;

# if statement 1 is true then set x(1) = 1, else 0
        lca1: y[1]-9*x[1] <= 0;

        lca2: y[1]-x[1] >= 0;

# ! if statement 2 is true then set x(2) = 1, else 0
        lcb1: y[2]-9*x[2] <= 0;
        lcb2: y[2]-x[2] >= 0;

#  ! if statement 3 is true then set x(3) = 1, else 0
        lcc1: sum{i in 1..person} x[i]+9*x[3] >= 2;
        lcc2: sum{i in 1..person} x[i]+9*x[3] <= 10;

 
#  ! display results
#  forall(i in 1..person) do
#    write(getsol(x(i)),' ',getsol(y(i)))
#    writeln
#  end-do


option solver cplex;
# option solver bonmin;
solve;

display x,y;
