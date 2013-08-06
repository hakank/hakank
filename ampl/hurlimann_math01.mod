/*
MODEL TOBACCO ALIAS T ;
  INTEGER VARIABLE x; y;
  CONSTRAINT A: 25*x+49*y = 2000;
  MINIMIZE any: x;
  WRITE IF T.stat=7 
    'A solution is: (x,y) = (%d,%d)' : x,y;
END


 This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
 See also my AMPL page: http://www.hakank.org/ampl/
 
*/


var x >= 0 integer;
var y >= 0 integer;

s.t. A: 25*x+49*y = 2000;
#        B: x >= 32;

minimize any: x;

option solver cplex;
solve;

display x,y;
printf "A solution is: (x,y) = (%d,%d)\n", x,y;

