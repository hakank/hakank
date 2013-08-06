/*

http://www.chlond.demon.co.uk/puzzles/puzzles2.html
"""
12. Joshua is a biology student. His project for this term is measuring the effect of an increase in vitamin C in the diet of nine laboratory rats. Each rat will have a different diet supplement of 1 to 20 units. Fractions of a unit are not possible.

To get the maximum value for his experiment, Joshua has decided that for any group of three rats the supplements should not be in arithmetic progression. In other words, for three rats chosen at random, the biggest supplement less the middle supplement should be different from the middle supplement less the smallest supplement. Thus, if two of the supplements were 7 and 13 units, no rat could have a supplement of 1, 10 or 19 units.

Find a set of supplements that Joshua could use. (Sole)    Solution 

"""

Solution should be: 1,2,6,7,9,14,15,18,20

Here:
x [*] :=
1   1
2   2
3   6
4   7
5   9
6  14
7  15
8  18
9  20
;

(Note: This is too large for my current demo version of AMPL.)

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/


param rat = 9;
set R = 1..rat;
var x{R} integer >= 1 <= 20;
var d{R,R,R} binary;

minimize tot:
   sum{i in R} x[i];

subject to ne{i in 2..rat}:
    x[i] >= x[i-1]+1;

subject to na{i in R,j in R,k in R : j < i and k < j}:
    -x[i]+2*x[j]-x[k]+19*d[i,j,k] >= 1;

subject to nb{i in R,j in R,k in R : j < i and k < j}:
    -x[i]+2*x[j]-x[k]+19*d[i,j,k] <= 18;


option solver cplex;
# option solver lpsolve;
# option solver cbc;
solve;
display x;
# display d;

for{i in R} {
  printf "%d ", x[i];
}
printf "\n";

#  forall(i in R)
#    write(getsol(x[i]),' ')


