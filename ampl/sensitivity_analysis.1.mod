/*

Thu Dec 27 20:42:54 2007/hakank@bonetmail.com

Sensitivity analysis,

Winston OR, page 232 ff

See http://www.student.math.uwaterloo.ca/~co370/ampl/ampl-cmd.txt
for a list of suffixes to use:


Standard suffix for _var:
Possible suffix values for _var.suffix:
        astatus   defeqn    derstage   dual
        init      init0     lb         lb0
        lb1       lb2       lrc        lslack
        no        rc        relax      slack
        sno       sstatus   stage      status
        ub        ub0       ub1        ub2
        urc       uslack    val

Standard suffix for _con:
Possible suffix values for _con.suffix:
        astatus   body      defvar   derstage
        dinit     dinit0    dual     lb
        lbs       lbs1      lbs2     ldual
        lslack    no        relax    slack
        sno       sstatus   stage    status
        ub        ubs       ubs1     ubs2
        udual     uslack


With option cplex_option "sensitivity";
there's also
  .current, .up och .down

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

var x1 >= 0;
var x2 >= 0;
var x3 >= 0;
var x4 >= 0;

maximize z: 4*x1 + 6*x2 + 7*x3 + 8*x4;

subject to 
        c1:   x1 +   x2 +   x3 +   x4  =  950;
        c2:                        x4 >=  400;
        c3: 2*x1 + 3*x2 + 4*x3 + 7*x4 <= 4600;
        c4: 3*x1 + 4*x2 + 5*x3 + 6*x4 <= 5000;

        


# data;

option cplex_options "sensitivity";
option solver cplex;
option presolve 0;
solve;
display _obj, _obj.relax;
display _varname, _var, _var.rc, _var.lb,_var.ub, _var.urc, _var.val;
# .current, down och up funkar endast för option cplex "sensitivity"!
# display _varname, _var, _var.rc,_var.lb, _var.ub, _var.slack, _var.lrc, _var.urc, _var.lb, _var.sstatus,_var.status, _var.down,_var.current,_var.up;
# display _varname, _var, _var.down, _var.current, _var.up;

display _conname, _con, _con.slack, _con.lb, _con.ub, _con.body, _con.status, _con.up, _con.down, _con.current;
display _conname, _con, _con.down, _con.current,_con.up;
display _conname, _con, _con.status, _con.lb,_con.ub;
