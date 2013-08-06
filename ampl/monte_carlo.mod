/*
http://www.lehigh.edu/~jtl3/teaching/ie495/

Run as
 $ ampl monte_carlo.run


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

param w1; 
param w2; 

param PENALTY := 5;

var x1 <= 2, >= 2;
var x2 <= 2, >= 2;

var y1 >= 0;
var y2 >= 0;

minimize ObjPlusRecourse:
         x1 + x2 + PENALTY * (y1 + y2);

subject to c1:
        w1 * x1 + x2 + y1 >= 7;

subject to c2:
        w2 * x1 + x2 + y2 >= 4;

