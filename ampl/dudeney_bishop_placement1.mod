/*

   Xpress-Mosel Model

   model 'dbishop1'

   ! Description  : Dudeney's bishop placement problem I
   ! Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.  
   ! Date written : Xpress-MP 26/10/99, Mosel 17/4/03
   ! Written by   : M J Chlond 

   http://www.chlond.demon.co.uk/puzzles/puzzles2.html
   """
   7. Place as few bishops as possible on an ordinary chessboard so that every 
   square of the board shall be either occupied or attacked. (Dudeney)
   """

Solution:
8 

0  0  0  0  0  0  0  0
0  0  0  0  0  1  0  0
0  0  1  0  0  0  0  0
0  0  1  0  0  0  1  0
0  0  0  0  0  0  1  0
0  0  1  1  0  0  1  0
0  0  0  0  0  0  0  0
0  0  0  0  0  0  0  0

Here:

0   0   0   0   0   0   0   0
0   0   0   1   0   0   0   0
0   0   0   0   0   0   0   0
0   0   0   1   0   0   1   0
0   1   0   1   0   0   1   0
0   0   0   1   0   0   1   0
0   0   0   0   0   0   0   0
0   0   0   0   0   0   0   0

This is not the same solution, but checking
x[i,j] + a[i,j] it seems to be correct:
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/



*/

param size = 8;

set S = 1..size;
var x{S,S} binary; # x(i,j) = 1 if square {I,J} occupied, 0 otherwise
var a{S,S} binary; # a(i,j) = 1 if square {I,J} attacked, 0 otherwise

# minimize the number of bishops
minimize numb:
         sum{i in S,j in S} x[i,j];

# a(i,j) = 0 if square {i,j} not attacked
s.t. att{i in S,j in S}:
   sum{m in S : m <> i and m-i+j >= 1 and m-i+j <= size} x[m,m-i+j]+
   sum{m in S : m <> i and i+j-m >= 1 and i+j-m <= size} x[m,i+j-m] >= a[i,j] ;

# each square either attacked or occupied
s.t. mb{i in S,j in S}:
   a[i,j]+x[i,j] = 1;

option solver cplex;
solve;

display numb;
display x;
display a;

# Check that all cells are either attacked or occupied: OK.
# 
for{i in S} {
   for{j in S} {
       printf "%d ", x[i,j] + a[i,j];
   }
   printf "\n";
}

