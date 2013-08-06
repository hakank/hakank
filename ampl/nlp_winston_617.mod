/*
  Wed Jan  2 21:33:42 2008/hakank@bonetmail.com

  Winston OR, sid 617 (nonlinear programming)


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

var K >= 0:= 1; 
var L >= 0:= 1;

maximize z: K*L;

subject to 
        c1: 4*K + L <= 8;


# data;

#option cplex_options "sensitivity writeprob=xxx.lp";
#option solver cplex;
option solver minos;
#option solver cbc;
# option solver snopt;
#option solver gjh;
#option solver ipopt;
# option solver bonmin;
#option solver lpsolve;
# option solver donlp2;
# option solver loqo;

solve;

display z;
display K,L;
# display _obj;
# display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
# display _conname, _con, _con.lb, _con.ub, _con.slack;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;
