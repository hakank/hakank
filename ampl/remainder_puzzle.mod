/*
  
  Remainder_puzzle in AMPL+CP.

  From
  http://www.chlond.demon.co.uk/puzzles/puzzles1.html
  """
  10. Is there a number which when divided by 3 gives a remainder of 1; when divided by 4, 
  gives a remainder of 2; when divided by 5, gives a remainder of 3; and when divided by 6, 
  gives a remainder of 4? (Kordemsky)
  """

  Answer: 58.


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/
   
*/

set INT = 1..4;
var x{INT} integer, >= 1;
var n integer, >= 1;     

s.t.  cona:
       -3*x[1] + n = 1;
s.t.  conb:
       -4*x[2] + n = 2;
s.t.  conc:
       -5*x[3] + n = 3;
s.t.  cond:
       -6*x[4] + n = 4;

minimize minnum:
         n;

option solver cplex;

solve;

display x, n;
