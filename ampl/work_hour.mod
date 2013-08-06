/*
 A completes a job in 5 hours. B can do the same job in 7.5 hours.
 How long will it take them if they work together?

 Solution:
 ========
 hour = 3


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

var hour;

minimize hours: hour;

subject to Time: hour / 5 + hour / 7.5 = 1;

/* option solver cplex; */
solve;
display hour;
