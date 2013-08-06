/*

  http://xkcd.com/287/

  A mix of these should sum to exactly 15.05:
  2.15, 2.75, 3.35, 3.55, 4.20, 5.80

x [*] :=
2.15  7
2.75  0
3.35  0
3.55  0
 4.2  0
 5.8  0
;

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/



*/


set price := {2.15, 2.75, 3.35, 3.55, 4.20, 5.80};
param total := 15.05; # original
# param total := 87.7; # testing alternative constraints

var x{price} integer >= 0;

minimize z:
#maximize z:
        sum{i in price} x[i];

s.t. c1:
        sum{i in price} x[i]*i = total;

# experimental constraints
# exactly n plates was ordered
# s.t. c2:
#         sum{i in price} x[i] = 21;

# all must be ordered at least once
#s.t. c3{i in price}:
#        x[i] >= 1
#;


option solver cplex;
#option solver lpsolve;
# option solver cbc;

solve;

display x;


display sum{i in price: x[i] >= 1} x[i];