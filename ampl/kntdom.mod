/*

0  0  0  0  0  0  0  0
0  0  0  1  0  1  0  0
0  1  0  1  0  1  0  0
0  1  0  0  0  1  0  0
0  1  0  0  0  1  0  0
0  1  0  1  0  1  0  0
0  0  0  1  0  1  0  0
0  0  0  0  0  0  0  0
Xpress-Mosel Model

model 'kntdom'

!  Description  : Knight domination puzzle - all squares threatened
!  Source       : M Kraitchik - Mathematical Recreations (P256)
!  Date written : MAGIC 16/12/92, Xpress-MP 15/6/98, Mosel 17/4/03
!  Written by   : M J Chlond

  uses 'mmxprs'

Xpress:
14
0 0 0 0 0 0 0 0 
0 0 1 0 1 1 0 0 
0 0 1 0 1 0 0 0 
0 0 1 0 0 0 1 0 
0 0 1 0 1 0 0 0 
0 1 1 0 1 1 1 0 
0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 


cplex:
minnum = 14

0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 1 1 1 1 1 1 0
0 0 0 0 0 0 0 0
0 1 1 0 0 1 1 0
0 0 0 0 0 0 0 0
0 0 1 1 1 1 0 0
0 0 0 0 0 0 0 0


   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/



*/

param rows := 8 ;
param cols := 8;
  
var x{1..rows+4,1..cols+4} binary;
var a{1..rows+4,1..cols+4};

minimize minnum: sum{i in 3..rows+2,j in 3..cols+2} x[i,j];

# Every real square threatened 
s.t. sq{i in 3..rows+2,j in 3..cols+2}:
        x[i-2,j-1]+x[i-1,j-2]+x[i+1,j-2]+x[i+2,j-1]+
        x[i+2,j+1]+x[i+1,j+2]+x[i-1,j+2]+x[i-2,j+1] >= 1 ;

# Dummy squares not occupied 
s.t. setzero: 
        sum{i in 1..2,j in 1..cols+4} x[i,j]+
        sum{i in rows+3..rows+4,j in 1..cols+4} x[i,j]+
        sum{j in 1..2,i in 3..rows+2} x[i,j]+
        sum{j in rows+3..rows+4,i in 3..rows+2} x[i,j] = 0;

option solver cplex;
# option solver bonmin;

solve;

display minnum;  

for{i in 3..rows+2} {
  for{j in 3..cols+2 } {
        printf "%d ",  x[i,j];
  }
  printf "\n";
}

