/*
  Tue Jan  8 19:57:54 2008/hakank@bonetmail.com

  Optimeringslaera, page 555 ff. Hanging chain.


Ipopt gives this solution (same as in the book page 558 modulo
the number of decimals).

z = -66.5465
y [*] :=
 1  -0.814795
 2  -0.782684
 3  -0.742825
 4  -0.693134
 5  -0.631138
 6  -0.554159
 7  -0.459806
 8  -0.346884
 9  -0.216639
10  -0.0737682
11   0.0737682
12   0.216639
13   0.346884
14   0.459806
15   0.554159
16   0.631138
17   0.693134
18   0.742825
19   0.782684
20   0.814795


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

# LP model from page 557
param n := 20;
param x := 16;
var y{1..n};

minimize z:
        sum{i in 1..n} (n-i+1/2)*y[i];

s.t. c1:
        sum{i in 1..n} y[i] = 0;


s.t. c2:
        - sum{i in 1..n} sqrt(1-y[i]^2) = -x;

#data;

#option presolve 0;
#option cplex_options "sensitivity";
#option solver cplex;
option bonmin_options 'bonmin.algorithm B-BB'; # simple branch-and-bound algorithm,
#option bonmin_options 'bonmin.algorithm B-OA'; # OA Decomposition algorithm,
# option bonmin_options 'bonmin.algorithm B-QG'; # Quesada and Grossmann branch-and-cut algorithm,
#  option bonmin_options 'bonmin.algorithm B-Hyb'; # hybrid outer approximation based branch-and-cut,

#option solver bonmin;
#option solver cbc;
#option solver donlp2;
#option solver gjh; 
#option solver ipopt; # ger rätt lösning
#option kestrel_options 'solver=xxx';
#option solver kestrel;
#option solver LaGO;
option solver loqo;
#option solver lpsolve;
#option solver minos;
#option solver pcx;
#option solver snopt;
#option solver umsip;

solve;

display z,y;

#display _obj;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
#display _conname, _con, _con.lb, _con.ub, _con.slack;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;
