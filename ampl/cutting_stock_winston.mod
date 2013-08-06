/*
  Wed Jan  2 20:35:34 2008/hakank@bonetmail.com

  Winston OR, page 570 cutting stock problem.

  """
  Woodco seels 3-ft, 5-ft, and 9-ft pieces of lumber. W:s customers demand
  25 3-ft, 20 5-ft and 15 9-ft boards. ... by cutting up 17-ft boards.
  """

Table over the different ways of cutting a 17-ft board:

Combination  3-ft   5-ft   9-ft   waste
1             5     0      0       2
2             4     1      0       0
3             2     2      0       1
4             2     0      1       2
5             1     1      1       0
6             0     3      0       2


LP-solution:
  x2 = 5/2, x5 = 15, x6 = 5/6

Integer-solution:
  x2 = 3, x5 = 15, x6=1

glpk give this with --intopt (not correct)
(z is 19)
x[1] = 1
x[2] = 0
x[3] = 3
x[4] = 0
x[5] = 15
x[6] = 0


   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/


*/

/*
# talkative version
var x1 >= 0 integer;
var x2 >= 0 integer;
var x3 >= 0 integer;
var x4 >= 0 integer;
var x5 >= 0 integer;
var x6 >= 0 integer;

minimize z: x1 + x2 + x3 +x4 +x5 +x6;

subject to
        c1: 5*x1 + 4*x2 + 2*x3 + 2*x4 + x5 >= 25; # 3-ft combinations
        c2: x2 + 2*x3 + x5 + 3*x6          >= 20;
        c3: x4 + x5                        >= 15;
*/

set cuts;
param num_comb >= 1;
param combinations{1..num_comb, cuts} >= 0;
param demand{cuts} >= 0;
var x{1..num_comb} >= 0 integer;

minimize z: 
        sum{i in 1..num_comb} x[i];

subject to c1{j in cuts}:
        sum{i in 1..num_comb} combinations[i,j]*x[i] >= demand[j];

data;

param num_comb := 6;
param combinations: 3 5 9 := 
        1    5     0      0
        2    4     1      0
        3    2     2      0
        4    2     0      1
        5    1     1      1
        6    0     3      0
        ;

param: cuts: demand :=
        3 25
        5 20
        9 15
;



option cplex_options "sensitivity writeprob=xxx.lp";
option solver cplex;
# option solver minos;
#option solver cbc;
# option solver snopt;
#option solver gjh;
# option solver ipopt;
# option solver bonmin;
# option solver lpsolve;
# option solver donlp2;
#  option solver loqo;

solve;

display c1;
display z;
display x;
# display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
# display _conname, _con, _con.lb, _con.ub, _con.slack;

