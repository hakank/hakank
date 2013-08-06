/*

   http://www.chlond.demon.co.uk/puzzles/puzzles2.html
   """
   8. What is the greatest number of bishops that can be placed on the chessboard 
   without any bishop attacking another? (Dudeney)
   """

   ! Description  : Dudeney's bishop placement problem II
   ! Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.  
   ! Date written : Xpress-MP 26/10/99, Mosel 17/4/03
   ! Written by   : M J Chlond 

14 

1  0  1  0  0  1  1  0
1  0  0  0  0  0  0  0
0  0  0  0  0  0  0  0
1  0  0  0  0  0  0  1
1  1  0  0  0  0  0  1
0  0  0  0  0  0  0  0
0  0  0  0  0  0  0  1
1  1  1  0  0  1  0  0

Here:
1   1   1   1   1   1   1   0
0   0   0   0   0   0   0   0
0   0   0   0   0   0   0   0
0   0   0   0   0   0   0   0
0   0   0   0   0   0   0   0
0   0   0   0   0   0   0   0
0   0   0   0   0   0   0   0
1   1   1   1   1   1   1   0

Which is a quite symmetric solution:


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/



param size = 8;
set S = 1..size;
var x{S,S} binary; # x(i,j) = 1 if square {I,J} occupied, 0 otherwise
var a{S,S} binary; # a(i,j) = 1 if square {I,J} attacked, 0 otherwise


# maximise number of bishops
maximize numb:
          sum{i in S,j in S} x[i,j];

# a[i,j] = 1 if square {i,j} attacked
s.t. att{i in S,j in S}:
   sum{m in S : m <> i and m-i+j >= 1 and m-i+j <= size} x[m,m-i+j]+
   sum{m in S : m <> i and i+j-m >= 1 and i+j-m <= size} x[m,i+j-m] <= 99*a[i,j] ;
   # varför 99 (ett löjligt högt tal?)


# each square either attacked or occupied
subject to mb{i in S,j in S}:
  a[i,j]+x[i,j] = 1;
    

option solver cplex;
# option solver cbc;
solve;

display x;
# display a;

# print "#:", sum{i in S, j in S} x[i,j];
print numb;

