/*
  Thu Jan  3 22:01:13 2008/hakank@bonetmail.com

  Winston OR, page 656 
  a non linear problem:
     monopolistic pricing with multiple customer types

  Solution: 
    q1 = 55/8 = 6.875 
    q2 = 9/2  = 4.5

  Which is the solution given by minos, cplex, donlp2 etc.
  bonmin gives 0,0


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

var q1 >= 0;# integer;
var q2 >= 0;# integer;

maximize z:
        q1*(70-4*q1) + q2*(150-15*q2) - 100 - 15*q1 - 15*q2;


# data;

#option presolve 0;
#option cplex_options "sensitivity";
option solver cplex;
#option solver bonmin;
#option solver cbc;
# option solver donlp2;
#option solver gjh;
#option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
# option solver loqo;
# option solver lpsolve;
#option solver minos;
#option solver pcx;
# option solver snopt;
#option solver umsip;

solve;

display q1, q2;

display _obj;
display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
display _conname, _con, _con.lb, _con.ub, _con.slack;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;
