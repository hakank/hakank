/*
  Williams "Model Building", page 131
  Non-linear model.


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

var x1 >= 0;
var x2 >= 0;

minimize z:
        x1^2 - 4*x1 - 2*x2;

c1: x1 + x2 <= 4;
c2: 2*x1 + x2 <= 5;
c3: -x1 + 4*x2 >= 2;

option show_stats 3;
#option solver cplex;
#option solver bonmin;
#option solver ipopt;
solve;

display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;#, _var.down, _var.current, _var.up, _var.dual;
display _conname, _con, _con.lb, _con.ub, _con.slack;#, _con.down, _con.current, _con.up, _con.dual,_con.status;
