/*

5 x 5 puzzle.


From http://www.chlond.demon.co.uk/Five.html (Java applet).
(Link from från http://www.chlond.demon.co.uk/puzzles/puzzles1.html)

"""
Each of the squares in the above grid can be in one of two states, lit(white) or unlit(red). If the player clicks on a square then that square and each orthogonal neighbour will toggle between the two states. Each mouse click constitutes one move and the objective of the puzzle is to light all 25 squares in the least number of moves.
"""

    
The answer (according to the XPress Mosel) should be

            1  0  1  1  0
            0  1  1  1  0
            1  1  1  0  0
            1  1  0  1  1
            0  0  0  1  1

but this model yields:

0 1 1 0 1
0 1 1 1 0
0 0 1 1 1
1 1 0 1 1
1 1 0 0 0

which - interestingly enough - is the same but 
with the columns reversed.

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

param n = 5;

set N = 1..n;
var x{N,N} binary;
var d{N,N} integer, >= 0;

subject to con{i in N,j in N}:
              (sum{k in j-1..j+1 : k >= 1 and k <= n and k != j} x[i,k]+
               sum{k in i-1..i+1 : k >= 1 and k <= n} x[k,j]) = 2*d[i,j]+1;


#  forall(i in N,j in N) do
#    x(i,j) is_binary
#    d(i,j) is_integer
#  end-do

minimize M:
         sum{i in N, j in N} x[i,j];

# option solver cplex;
option solver cbc;
solve;

display x;
display d;
display M;

for{i in N} {
   for {j in N} {
     printf "%d ", x[i,j];
   }
   printf "\n";
};

print "Answer (according to the XPress Mosel model) should be:";
print "            1  0  1  1  0";
print "            0  1  1  1  0";
print "            1  1  1  0  0";
print "            1  1  0  1  1";
print "            0  0  0  1  1";
