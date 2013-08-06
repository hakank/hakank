/*
  Mon Jan  7 20:07:10 2008/hakank@bonetmail.com

  Optimeringslaera page 288ff: cutting patterns of round plates

  The book has no specific values so I fabulate a little.

param r :=
        1 1
        2 2
        3 3
        4 4
        5 5
;

SNOPT 6.1-1: Optimal solution found.
162 iterations, objective 1483.210298
Nonlin evals: obj = 101, grad = 100, constrs = 101, Jac = 100.
:      x          y        :=
1    6.02859    6.01182
2   12.0944    11.2702
3   24.9642     8
4    9         21.8452
5   22.9642    20.8452
;

w1 = 17.0358
w2 = 19.1548
z = 1483.21

MINOS 5.5: infeasible problem (or bad starting guess).
96 iterations
Nonlin evals: obj = 101, grad = 100, constrs = 101, Jac = 100.
:)
:     x     y     :=
1    6       6
2    7       7
3   27.75    8
4   41       9
5   40      40
;

w1 = 0
w2 = 0
z = 0

bonmin: Optimal
:      x         y       :=
1    6         6
2    7        14.1337
3   14.898     8
4   26.1635   12.1337
5   40        10
;

w1 = 0
w2 = 28.8663
z = 1443.32


Ipopt:
Ipopt 3.2.4s: Optimal Solution Found
:      x          y       :=
1    7.26073    6
2    7         13.9957
3   14.3726    20.7519
4   16.8001     9
5   27.1609    18.4156
;

w1 = 12.8391
w2 = 21.2481
z = 1431.55

bonmin with integer-constraint on x,y,w1 och w2:
bonmin: Optimal
:   x    y     :=
1    6    6
2    7   15
3   15    8
4   26   13
5   40   10
;

w1 = 0
w2 = 28
z = 1400

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

param n >= 1;
param L >= 0;
param B >= 0;
param d >= 0;
param r{1..n};
param p;

var x{1..n} >= 0;# integer;
var y{1..n} >= 0;# integer;
var w1 >= 0;# integer;
var w2 >= 0;# integer;

data;

param n := 5;
param L := 50;
param B := 50;
param d := 5;
param p := 10;

# radien för respektive lock
param r :=
        1 1
        2 2
        3 3
        4 4
        5 5
;

# minimera outnyttjat mellan hålen
# (kvadraten w1 och w2 kan återutnyttjas)
maximize z:
        p*(B*w1 + L*w2 - w1*w2);

s.t. c1{i in 1..n, j in 1..n: i <> j}:
        (x[i] -x[j])^2 + (y[i] - y[j])^2 >= (r[i] + r[j] + d)^2;

s.t. c2{i in 1..n}:
        x[i] + w1 <= L -r[i] -d;

s.t. c3{i in 1..n}:
        y[i] + w2 <= B -r[i] -d;

s.t. c4{i in 1..n}:
        x[i] >= r[i] + d;

s.t. c5{i in 1..n}:
        y[i] >= r[i] + d;


# option presolve 0;
# för att skriva problem till en .lp-fil sätt writeprob=xxx.lp
#option cplex_options "sensitivity";
# option solver cplex;
#option solver bonmin;
# option solver cbc;
# option solver donlp2;
# option solver gjh;
#option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
# option solver LaGO;
#option solver loqo;
#option solver lpsolve;
#option solver minos;
#option solver pcx;
#option solver snopt;
#option solver umsip;
#option solver pswarm;

# testar med startpunkter > 0
for{i in 1..n} {
  let x[i] := i*1.1;
  let y[i] := i*1.1;
}

solve;

display x, y,w1,w2,z;

#display _obj;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
#display _conname, _con, _con.lb, _con.ub, _con.slack;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;
