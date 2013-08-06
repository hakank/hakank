/*
Door 1 - tiger 
<br>
Door 2 - lady 

Xpress-Mosel Model

model 'trial1'

! Description  : The First Trial
! Source       : Smullyan, R., (1991), The Lady or The Tiger, Oxford University Press
! Date written : Xpress-MP 5/12/99, Mosel 19/4/03
! Written by   : M J Chlond 

  uses 'mmxprs'
  
Xpress:
0 1 
1 0 
0
1

cplex:
0 1
1 0
0
1


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/



*/

param door  := 2;
param prize := 2; # 1 = Lady, 2 = Tiger

set D := 1..door;
set P := 1..prize;
var x{D,P} binary;  # x(i,j) = 1 if door i hides prize j, else 0
var t{D} binary;    # t(i) = 1 if statement on door i is true, else 0
var d{1..2} binary; # dummy variables

minimize any: x[1,1];

# each door hides 1 prize
s.t. pca{i in D}:
        sum{j in P} x[i,j] = 1;

# if statement on door 1 is true then set t[1] = 1, else t[1] = 0
s.t. lca1: x[1,1]+x[2,2]-t[1] <= 1;

   lca2: x[1,1]+x[2,2]-2*t[1] >= 0;

# if statement on door 2 is true then set t[2] = 1, else t[2] = 0
  lcb1: x[1,1]-x[2,2]-2*d[1] <= -1 ;
  lcb2: x[1,1]-x[2,2]-d[1] >= -1;

  lcb3: x[1,1]-x[2,2]+2*d[2] >= 1;
  lcb4: x[1,1]-x[2,2]+2*d[2] <= 2;

  lcb5: d[1]+d[2]-t[2] <= 1;
  lcb6: d[1]+d[2]-2*t[2] >= 0;

# only one statement true
  lcc: t[1]+t[2] = 1;

option solver cplex;

solve;

for{i in D} {
  for{j in P} {
     printf "%d ", x[i,j];
  }
  printf "\n";
}

for{i in D} {
   printf "%d\n", t[i];
}
printf "\n";
