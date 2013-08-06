/*
  Fri Jan 11 22:26:16 2008/hakank@bonetmail.com

  Hurlimann page 43.


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

var x integer >= 0;
var y integer >= 0;

minimize z: 0;

s.t. a: 25*x + 49*y = 2000; 

s.t. b: x <= 31;

# data;

#option presolve 0;
# to write the problem to a .lp file:
#  writeprob=xxx.lp
# option solver cplex;
#option solver bonmin;
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

display x, y;
