
/*
 Gomory's cut.

  http://www.ms.unimelb.edu.au/~moshe/620-362/gomory/index.html

Original-problem:

z*: = max z = 5x1 + 8x2
s.t.
x1 + x2 <= 6
5x1 + 9x2 <= 45
x1, x2 >=0, and integer


rewritten as:

z*: = max z = 5x1 + 8x2
s.t. 
  x1 +   x2 + s1       = 6
  5x1 + 9x2 + s2 = 45
  x1, x2, s1, s2 >=0, and integer

where s1 och s2 are slack variables


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

var x1 integer >= 0;
var x2 integer >= 0;
var s1 integer >= 0;
var s2 integer >= 0;


maximize z:
        5*x1 + 8*x2;

# original problem
# y1: x1 + x2 <= 6;
# y2: 5*x1 + 9*x2 <= 45;

# rewrites with slack s1 and s2:
# note: "<=" is rewriten as "="
# and s1, s2 are integers that takes the "rest"
y1: x1 + x2 + s1 = 6;
y2: 5*x1 + 9*x2 + s2 = 45;


# option solver lpsolve;
solve;


# display x1,x2,z;
# printf "x1:%d x2:%d\n", x1,x2;

display x1,x2,s1,s2,z;
printf "x1:%d x2:%d s1:%d s2:%d\n", x1,x2,s1,s2;

end;


        