/*

http://www.chlond.demon.co.uk/puzzles/puzzles1.html
"""
5. A side show at Coney Island is described as follows: "There were ten little 
dummies which you were to knock over with baseballs. The man said: 'Take as many 
throws as you like at a cent apiece and stand as close as you please. Add up the 
numbers on all the men that you knock down and when the sum amounts to exactly 
fifty, neither more nor less you get a genuine 25 cent Maggie Cline cigar with 
a gold band around it.'"

The numbers on the ten dummies were 15, 9, 30, 21, 19, 3, 12, 6, 25, 27. (Loyd)
"""

Answer: 6, 19 and 25

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

param n = 10;

set N = 1..n;
param v{N};
var x{N} binary;

data;

param v:=
  1 3
  2 6
  3 9
  4 12
  5 15
  6 19
  7 21
  8 25
  9 27
  10 30;

minimize value:
          sum{i in N} x[i];

subject to fifty: 
        sum{i in N} v[i]*x[i] = 50;

#option solver cplex;
option solver lpsolve;
solve;

# display x;

# show only the relevant
display {i in N : v[i]*x[i] > 0} v[i]*x[i];

# do this automatically
option omit_zero_rows 1;
display x;
display {i in N} v[i]*x[i];
