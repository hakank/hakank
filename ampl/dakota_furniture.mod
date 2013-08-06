/*

   Fri Dec 28 11:37:09 2007/hakank@bonetmail.com

   One of the standard examples from Winston OR, e.g. page 140.


   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/

*/

var x1 >= 0; # desks
var x2 >= 0; # table
var x3 >= 0; # chairs

maximize z:
        60*x1 + 30*x2 + 20*x3;

subject to
        lumber: 8*x1 + 6*x2+x3 <= 48; 
        finishing: 4*x1 + 2*x2 + 1.5*x3 <= 20;
        carpentry: 2*x1 + 1.5*x2 + 0.5*x3 <= 8;
        table_demand: x2 <= 5;
        


# data;

option cplex_option "sensitivity";
option solver cplex;
solve;
display x1, x2, x3;
display _obj;
display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack,_var.dual;
display _conname, _con, _con.lb, _con.ub, _con.slack,_con.dual;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;
