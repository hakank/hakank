/*
  Tue Mar 18 20:11:53 2008/hakank@bonetmail.com

  Transportation problem.

 
cplex:
:  _varname   _var _var.rc _var.lb _var.ub _var.slack    :=
1    A1          0     0       0       200        0
2    A2        100     0       0       200      100
3    A3        100     0       0       200      100
4    B1        200     0       0       400      200
5    B2        200     0       0       300      100
6    B3          0     0       0       400        0
7    C1        300     0       0       300        0
8    C2          0     0       0       300        0
9    C3          0     0       0       300        0
10   D1          0     0       0       100        0
11   D2          0     0       0       100        0
12   D3        100     0       0       100        0
13   Obj      6600     1       0     21600     6600

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

var A1 integer >= 0;
var A2 integer >= 0;
var A3 integer >= 0;
var B1 integer >= 0;
var B2 integer >= 0;
var B3 integer >= 0;
var C1 integer >= 0; 
var C2 integer >= 0;
var C3 integer >= 0;
var D1 integer >= 0;
var D2 integer >= 0;
var D3 integer >= 0;

var Obj integer >= 0;

minimize z: Obj;

c1: A1 + A2 + A3 = 200;
c2: B1 + B2 + B3 = 400;
c3: C1 + C2 + C3 = 300;
c4: D1 + D2 + D3 = 100;

c5: A1 + B1 + C1 + D1 <= 500;
c6: A2 + B2 + C2 + D2 <= 300;
c7: A3 + B3 + C3 + D3 <= 400;

c8: Obj =  10*A1 + 7*A2 + 11*A3 +
           8*B1 + 5*B2 +10*B3+
           5*C1 + 5*C2 + 8*C3+
           9*D1 + 3*D2 + 7*D3;

option solver cplex;
solve;

display _obj;
display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
display _conname, _con, _con.lb, _con.ub, _con.slack;
