/*
trial2 från Chlonds artikel

IP Modeling and the Logical Puzzles of Raymond Smullyan
http://ite.pubs.informs.org/Vol3No3/ChlondToase/
 
Koden:
http://ite.pubs.informs.org/Vol3No3/ChlondToase/trial2.php


model 'trial2'

! Description  : The Second Trial
! Source       : Smullyan, R., (1991), The Lady or The Tiger, OUP
! Date written : 6/12/99
! Written by   : M J Chlond

Interpretation of output

The 'display results' section produces the following output.

x =	0 1 
	1 0 
	
t =	1 1 

Thus, door one hides the tiger, door two hides the lady, and the statements on both doors are true.


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

 
*/

param door  = 2;
param prize = 2;  # 1 = Lady, 2 = Tiger

var x{1..door,1..prize} binary; # x(i,j) = 1 if door i hides prize j, else 0
var t{1..door} binary;        #  t(i) = 1 if statement on door i is true, else 0

minimize any: x[1,1];

#! each door hides 1 prize
subject to pca{i in 1..door}:
        sum{j in 1..prize} x[i,j] = 1;

# if statement on door 1 is true then t[1] = 1, else t[1] = 0
        lca1: x[1,1]+x[2,1]-2*t[1] <= 0 ;
        lca2: x[1,1]+x[2,1]-t[1] >= 0;

# if statement on door 2 is true then t[2] = 1, else t[2] = 0
        lcb: t[2] = x[1,2];
        
# statements either both true or both false
        lcc: t[1] = t[2];

option solver cplex;
solve;

# display x, t;

print "x:";
for{i in 1..door} {
        for{j in 1..prize} {
           printf "%d ", x[i,j];
        }
        printf "\n";
}

print "t:";
for {i in 1..door}
        printf "%d\n", t[i];
printf "\n";