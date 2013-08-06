/*
     ~/linear_programming/xpress/examples/puzzles/sol1s8.html.mos
  Chlond problem 1 8:
Answer should be:
0  0  0  0  0  0  0  0
0  0  0  1  0  1  0  0
0  1  0  1  0  1  0  0
0  1  0  0  0  1  0  0
0  1  0  0  0  1  0  0
0  1  0  1  0  1  0  0
0  0  0  1  0  1  0  0
0  0  0  0  0  0  0  0

I get the following solution:

0  0  0  0  0  0  0  0
0  0  1  0  1  0  0  0
0  0  1  0  1  0  1  0
0  0  1  0  0  0  1  0
0  0  1  0  0  0  1  0
0  0  1  0  1  0  1  0
0  0  1  0  1  0  0  0
0  0  0  0  0  0  0  0


With transpose (without column reverse):

0  0  0  0  0  0  0  0
0  0  0  1  0  1  0  0
0  1  0  1  0  1  0  0
0  1  0  0  0  1  0  0
0  1  0  0  0  1  0  0
0  1  0  1  0  1  0  0
0  0  0  1  0  1  0  0
0  0  0  0  0  0  0  0

   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/


*/

param rows = 8;
param cols = 8;

var x{1..rows+4,1..cols+4} binary;
# var a{1..rows+4,1..cols+4}; # 

minimize minnum:
  sum{i in 3..rows+2,j in 3..cols+2} x[i,j];

# Every real square threatened
subject to sq{i in 3..rows+2,j in 3..cols+2}:
    x[i-2,j-1]+x[i-1,j-2]+x[i+1,j-2]+x[i+2,j-1]+
    x[i+2,j+1]+x[i+1,j+2]+x[i-1,j+2]+x[i-2,j+1] >= 1;

# Dummy squares not occupied
subject to setzero:
         sum{i in 1..2,j in 1..cols+4} x[i,j]+
         sum{i in rows+3..rows+4,j in 1..cols+4} x[i,j]+
         sum{j in 1..2,i in 3..rows+2} x[i,j]+
         sum{j in rows+3..rows+4,i in 3..rows+2} x[i,j] = 0;

option solver cplex;
# option solver cbc;
solve;

display minnum;

# display x;

for{i in 3..rows+2} {
    for{j in 3..cols+2} {
      printf "%d  ",x[i,j];
    }
    printf "\n";
}


print "Chlonds answer:";
print "0  0  0  0  0  0  0  0";
print "0  0  0  1  0  1  0  0";
print "0  1  0  1  0  1  0  0";
print "0  1  0  0  0  1  0  0";
print "0  1  0  0  0  1  0  0";
print "0  1  0  1  0  1  0  0";
print "0  0  0  1  0  1  0  0";
print "0  0  0  0  0  0  0  0";
