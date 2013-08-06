/*
  From Game Theory Decisions Interaction and Evolution, page 4


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

var n integer >= 0;

maximize z: 
        1-n^2+(9/2)*n;

option solver cplex;
# option solver donlp2;
# option solver bonmin;
solve;

display z;
display n;


