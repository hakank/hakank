#
# Giapetto's problem
#
# This finds the optimal solution for maximizing Giapetto's profit
#
# From 
# The GNU Linear Programming Kit, Part 1: Introduction to linear optimization
# http://www-128.ibm.com/developerworks/library/l-glpk1/index.html?ca=drs-
#
#
# This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my AMPL page: http://www.hakank.org/ampl/


#
/* Decision variables */
var x1 >=0;  /* soldier */
var x2 >=0;  /* train */

/* Objective function */
maximize z: 3*x1 + 2*x2;

/* Constraints */
s.t. Finishing : 2*x1 + x2 <= 100;
s.t. Carpentry : x1 + x2 <= 80;
s.t. Demand    : x1 <= 40;

# option solver minos;
# option solver cplex;

solve;

display x1, x2;

# option nl_comments 1;
# write ggiapetto;

end;

