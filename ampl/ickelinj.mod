/* 

  From http://www.mai.liu.se/~olbur/kurser/NMAB18/labinfo_1.pdf


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/
var x{1..2};
minimize obj: 2*x[1]^2 + x[1]*x[2] + 3*x[1] + 4*x[2]^2;

s.t. con1: x[1] + 2*x[2]^2 >= 3;
     con2: x[1] + 3*x[2] = 5;

data; # Startpunkt
var x :=
1 4
2 5;

solve;

display _objname, _obj; # Objective
display _varname, _var; # Variables
display _conname, _con.slack, _con.dual;
