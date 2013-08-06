/*
  Thu Jan  3 21:24:24 2008/hakank@bonetmail.com

  Winston OR, page 623. warehouse location
  (Non-linear problem)

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

# verbose version
/*
var x >=0 := 1;
var y >=0 := 1;
#var d1>=0;
#var d2>=0;
#var d3>=0;
#var d4>=0;

# minimize z: 200*d1 + 150*d2 + 200*d3 + 300*d4;
minimize z:
        200*(( x -  5)^2 + (y - 10)^2)^0.5 + 
        150*(( x - 10)^2 + (y -  5)^2)^0.5 +
        200*(( x -  0)^2 + (y - 12)^2)^0.5 + 
        300*(( x - 12)^2 + (y -  0)^2)^0.5;

# data;
# Give wrong solution.
# let d1 := (( x -  5)^2 + (y - 10)^2)^0.5;
#let d2 := (( x - 10)^2 + (y -  5)^2)^0.5;
#let d3 := (( x -  0)^2 + (y - 12)^2)^0.5;
#let d4 := (( x - 12)^2 + (y -  0)^2)^0.5;
*/

# General approach
set warehouse;
var x >= 0; # integer;
var y >= 0; # integer;
param coord{warehouse, 1..2} >= 0; # x,y coord
param shipments{warehouse};

minimize z:
        sum{i in warehouse} shipments[i]*(( x - coord[i,1])^2 + (y - coord[i,2])^2)^.5;

data;

param: warehouse: shipments :=
        1 200
        2 150
        3 200
        4 300
#        5 100  # test with one more
;

param coord: 1 2 :=
        1 5 10
        2 10 5
        3 0 12
        4 12 0
#        5 1 1
;



#option presolve 0;
#option cplex_options "sensitivity";
#option solver cplex;
#option solver bonmin;
# option solver cbc;
option solver donlp2;
#option solver gjh;
#option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
#option solver loqo;
# option solver lpsolve;
#option solver minos;
#option solver pcx;
#option solver snopt;
#option solver umsip;

solve;

display _obj;
display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
display _conname, _con, _con.lb, _con.ub, _con.slack;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;
expand;