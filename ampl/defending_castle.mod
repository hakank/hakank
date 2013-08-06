/*
  Fri Jan 11 07:40:25 2008/hakank@bonetmail.com

   http://www.cut-the-knot.org/do_you_know/lin_pr.shtml
   Assume that we have decided to position defenders of a square castle 
   according to the following plan:

     p q p
     q 0 q
     p q p

   so that the total number of defenders is 4(p+q) while (2p+q) fighters 
   face the enemy on every side. Let's denote p = x1 and q = x2. Let also 
   A = (4 4), be a 1×2 matrix, and c = (2 1), a 1×2 row vector. Assume that a 
   1×1 vector b is equal to (28). The linear programming problem (P) is then 
   interpreted as:
   How to position 28 fighters according to the symmetric arrangement above 
   so as to have the maximum number of defenders on each side?


  The constraint:
  4*(p + q) = 28;


First solution:
7 0 7
0 0 0
7 0 7

If the constraint q >= p (and integers) are added:
3 4 3
4 0 4
3 4 3

If q >= 2*p:
2 5 2
5 0 5
2 5 2

q >= 3*p:
1 6 1
6 0 6
1 6 1

q >= 15*p:
0 7 0
7 0 7
0 7 0


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/



*/

var p >= 0 integer;
var q >= 0 integer;

maximize z: 2*p + q;

s.t. c1: 4*(p + q) = 28;
     c2: q >= p;

# data;

#option presolve 0;
option cplex_options "sensitivity";
# option solver cplex;
option solver gurobi;
#option solver bonmin;
#option solver cbc;
#option solver donlp2;
#option solver gjh;
#option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
#option solver LaGO;
#option solver loqo;
#option solver lpsolve;
#option solver minos;
#option solver pcx;
#option solver snopt;
#option solver umsip;

solve;

display p,q;

print p,q,p;
print q,0,q;
print p,q,p;


display _obj;
display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
display _conname, _con, _con.lb, _con.ub, _con.slack;
#display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;
