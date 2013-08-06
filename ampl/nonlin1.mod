/*
   From Nonlinear Programming, Bazaras et al
   page 3

   Minimize (x1-3)^2 + (x2-2)^2
   Subject to 
       x1^2 - x2 -3 <= 0
       x2 - 1 <= 0
       -x1 <= 0


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/



*/

var x1;
var x2;

minimize opt: (x1-3)^2 + (x2-2)^2;

subject to A: x1^2 - x2 -3 <= 0;
subject to B: x2 - 1 <= 0;

subject to C: -x1 <= 0;

solve;

display x1, x2;
expand;

