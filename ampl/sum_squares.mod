/* 

The sum of two numbers is 15 and the sum of their squares is 113.
Find the numbers.

Solution:
========
x1 = 8
x2 = 7

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/
var x1;
var x2;

minimize t: x1;

subject to c1: x1 + x2 = 15;
subject to c2: x1*x1 + x2*x2 = 113;

solve;
display x1, x2;

end;
