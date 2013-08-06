/*
  Mon Dec 31 23:20:11 2007/hakank@bonetmail.com

  From Taha, OR, page 2
  The optimal solution is 
    w = h = L/4.


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

var L >= 0;
var w >= 0;
var h >= 0;

maximize z: w*h;

subject to 
        c1: 2*(w+h) = L;
        c2: L <= 100;
        c3: h >= 2;
           


data;

# let w := 10;

#option cplex_option "sensitivity";
# option solver cplex;
# option solver bonmin;
#option solver donlp2;

solve;
display _obj;
display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
display _conname, _con, _con.lb, _con.ub, _con.slack;
