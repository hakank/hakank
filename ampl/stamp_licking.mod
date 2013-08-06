/*

http://www.chlond.demon.co.uk/puzzles/puzzles4.html
"""
4. The Gentle Art of Stamp-licking

If you have a card divided into sixteen spaces (4 x 4), and are provided with plenty of stamps of the values 1d., 2d., 3d., 4d., and 5d., what is the greatest value that you can stick on the card if the Chancellor of the Exchequer forbids that you place any stamp in a straight line (that is, horizontally, vertically, or diagonally) with another stamp of similar value? Of course, only one stamp can be affixed in a space. (Dudeney 1917)    Solution
"""

Xpress-Mosel Model

model 'stamp'

!  Description  : The gentle art of stamp-licking
!  Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.
!  Date written : Xpress-MP May 2000, Mosel 19/4/03
!  Written by   : M J Chlond

Total 50, arranged as follows:

    4  5  1  3
    3  2  4  5
    5  1  3  2
    2  4  5  1


Here:
value = 50

3 4 5 1
5 1 2 3
2 3 4 5
4 5 1 2

Not as Chlond's version, but there are many different solutions...


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

param size = 4;
param stamp = 5;

set S = 1..size;
set T = 1..stamp;
var x{S,S,T} binary; # X(i,j,k)=1 if cell {i,j} has stamp value k, 0 otherwise
var a{S,S,T} binary;

maximize value:
        sum{i in S,j in S,k in T} k*x[i,j,k];

subject to ones{i in S,j in S}:
        sum{k in T} x[i,j,k] <= 1;

    # a(i,j,k) = 1 if stamp on square {i,j} is in line with similar stamp
subject to sta{i in S,j in S,k in T}:
        sum{m in S : m <> i and m-i+j >= 1 and m-i+j <= size} x[m,m-i+j,k]+
        sum{m in S : m <> i and i+j-m >= 1 and i+j-m <= size} x[m,i+j-m,k]+
        sum{m in S : m<> i} x[m,j,k] + sum{m in S : m <> j} x[i,m,k] <= 99*a[i,j,k]
;  

# square not both occupied and attacked by same stamp value
subject to ma{i in S,j in S,k in T}:
        a[i,j,k]+x[i,j,k] <= 1;


option solver cplex;
# option solver bonmin; # tar lång tid!
solve;

display value;
# display x;


for{i in S} {
   for{j in S} {
     printf "%d ", sum{k in T} k*x[i,j,k];
   }
   printf "\n";
}

#  forall(i in S) do
#    forall(j in S)
#      write(sum(k in T) k*getsol(x(i,j,k)),' ')
#    writeln
#  end-do
