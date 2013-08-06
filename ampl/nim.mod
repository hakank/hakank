/*
http://ite.pubs.informs.org/Vol3No3/ChlondAkyol/
A Nimatron
Martin J.Chlond


Xpress-kod:
http://ite.pubs.informs.org/Vol3No3/ChlondAkyol/Nimxp.php


model 'nim'

! nim.mos : Computes move to safe position (if available) in game of Nim
! Written by : Martin J. Chlond
! Date written : 26 December 2001

Output:
Solution:
0 5
0 4
0 3
0 2
1 0


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/



*/


param heap = 5; #" ! number of heaps
param col = 4; # ! columns for binary representation of position after move
param k = 1; # ! maximum number of heaps to change (if k>1 then Moore's game) ! number of squares per side

param nmax=2^col-1; # ! maximum number allowed in any heap
param n{1..heap}; # number in each heap before move;

# !variables
var x{1..heap,1..col} binary; # binary values of position after move
var d{1..heap} integer >= 0; # 1 if heap changed, 0 otherwise
var s{1..heap} integer >= 0; # number taken from heap
var m{1..heap} integer >= 0; # number in each heap after move
var w{1..col} integer >= 0; #  dummy variables for winning position test




# ! number of heaps changed
minimize heapch: sum{i in 1..heap} d[i];

# ! convert heap numbers [after move] to binary
subject to 

        conb{i in 1..heap}:
        sum{j in 1..col} 2^(j-1)*x[i,j] = m[i];

# ensures safe position after move
        winp{j in 1..col}:
        sum{i in 1..heap} x[i,j] = (k+1)*w[j];

# positions before and after are consistent with move
        cons{i in 1..heap}:
        n[i]-s[i] = m[i];

# dummy set to 1 if heap changed
        dset{i in 1..heap}:
        s[i]-nmax*d[i] <= 0;


# minimise number of heaps changed - if solution is zero then current position already safe
# minimise[ heapch ]

data;
param n :=
        1 5,
        2 4,
        3 3,
        4 2,
        5 1;


option solver cplex;
# option solver bonmin;

solve;

print "Solution:";
for{i in 1..heap} {
  printf "%d %d\n", s[i], m[i];
}
printf "\n";

# output solution
#forall[i in 1..heap] do
#write[getsol[s[i]],' ',getsol[m[i]]]
#writeln
#end-do

