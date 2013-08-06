/*
MODEL EGGS ;
  SET i := /1:6/;
  INTEGER VARIABLE X{i}; N;
  CONSTRAINT A{i}: N = (i+1)*X + IF(i<#i,1);
  MINIMIZE obj: N;
  WRITE 'There were at least %3d eggs in the basket.': N;
END

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

set i := 1..6;
var X{i} >= 0 integer; 
var N >= 0 integer;


s.t. A{k in i}: N = (k+1)*X[k] + if k<card(i) then 1;

minimize obj: N;

option solver cplex;
solve;

display X,N;


printf "There were at least %3d eggs in the basket.\n", N;
      