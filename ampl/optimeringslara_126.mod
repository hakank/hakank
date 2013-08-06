/*
  Sat Jan  5 09:45:22 2008/hakank@bonetmail.com

  Optimeringslaera, page 126ff,  sensitivity analysis

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

var xa1 >= 0;
var xa2 >= 0;
var xb1 >= 0;
var xb2 >= 0;


minimize z:
        330*xa1 + 340*xa2 + 285*xb1 + 295*xb2;

subject to
        c1:  xa1 + xa2 <= 200;
        c2:  xb1 + xb2 <= 100;
        c3: 4*xa1+2*xa2 + 3*xb1 + 2*xb2 >= 1000;
        c4: 4*xa1 + 6*xa2 + 3*xb1 + 4*xb2 >= 1100;




# data;

#option presolve 0;
option cplex_options "sensitivity";
option solver cplex;
# option solver bonmin;
#option solver cbc;
#option solver donlp2;
#option solver gjh;
# option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
# option solver LaGO;
#option solver loqo;
#option solver lpsolve;
#option solver minos;
#option solver pcx;
#option solver snopt;
#option solver umsip;

solve;

display z, xa1, xa2, xb1, xb2;
#display _obj;
# display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
/*
display _varname,  _var.astatus   , _var.defeqn    , _var.derstage   , _var.dual
        , _var.init      , _var.init0     , _var.lb         , _var.lb0
        , _var.lb1       , _var.lb2       , _var.lrc        , _var.lslack
        , _var.no        , _var.rc        , _var.relax      , _var.slack
        , _var.sno       , _var.sstatus   , _var.stage      , _var.status
        , _var.ub        , _var.ub0       , _var.ub1        , _var.ub2
        , _var.urc       , _var.uslack    , _var.val;
*/
#display _conname, _con, _con.lb, _con.ub, _con.slack;
/*
display _conname, _con.astatus   , _con.body      , _con.defvar   , _con.derstage
        , _con.dinit     , _con.dinit0    , _con.dual     , _con.lb
        , _con.lbs       , _con.lbs1      , _con.lbs2     , _con.ldual
        , _con.lslack    , _con.no        , _con.relax    , _con.slack
        , _con.sno       , _con.sstatus   , _con.stage    , _con.status
        , _con.ub        , _con.ubs       , _con.ubs1     , _con.ubs2
        , _con.udual     , _con.uslack;
*/

display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;
