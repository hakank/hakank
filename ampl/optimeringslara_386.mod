/*
  Mon Jan  7 20:49:05 2008/hakank@bonetmail.com

  Optimeringslaera page 386, integer programming.
  An example.

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

var x1 >= 0 integer;
var x2 >= 0 integer;

maximize z: x1 + x2;

s.t. c1:
        11*x1 - 8*x2 <= 22;
     c2:
        11*x1 - 12*x2 >= 0;

# data;

#option presolve 0;
#option cplex_options "sensitivity";
option solver cplex;
# option solver bonmin;
#option solver cbc;
#option solver donlp2;
#option solver gjh;
#option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
#option solver LaGO;
#option solver loqo;
#option solver lpsolve;
#option solver minos;
#option solver pcx;
#option solver snopt;
#option solver umsip;

solve;

display x1,x2,z;