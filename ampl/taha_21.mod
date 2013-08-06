/*
  Fri Jan 11 07:05:00 2008/hakank@bonetmail.com

  Taha "OR", page 21 (ex. 2.22)


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/


var work >= 0;
var play >= 0;

maximize fun:
        work + 2*play;


s.t.
        total_time: work + play <= 10;
        atleastasmuchwork: work >= play;
        play_4_hours: play <= 4;


option cplex_options "sensitivity";
option solver cplex;
solve;

display work, play;
display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;

