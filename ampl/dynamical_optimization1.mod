/*
   Dynamical Optimization

   From

   http://www.dcsc.tudelft.nl/%7Eahuesman/Ampl.pdf
   http://www.dcsc.tudelft.nl/~ahuesman/

   Solve the  problem
     min u(t) int(x dt, 0,10)
     s.t. 
         dx/dt = -x(t) + u(t)
         x(0) = 1
         x(10) = 5
         0 <= u(t) <= 10  


   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/


*/



# DOP1.mod - Simple problem to test dynamic optimization in AMPL format
# Adrie Huesman 27/11/2004
# Problem taken from "Research note: Solving and optimization of ODE.s"
# SETS
# no set declaration necessary!

# PARAMETERS
param Ng > 0 integer; # number of grid points
param Dt > 0; # time step

# VARIABLES
var x{1..Ng} >= -10, := 0.5, <= 10; # discrete state variable
var u{1..Ng} >= -0, := 0.1, <= 10; # discrete input variable

# OBJECTIVE
minimize cost: sum {j in 1..Ng} x[j]; # discrete integral approximation

# CONSTRAINTS
s.t. disx {j in 1..Ng-1}: (x[j+1] - x[j])/Dt = -x[j+1] + u[j+1];

# implicit Euler approximation
s.t. intx {j in 1..1}: x[j] = 1; # initial condition
s.t. finx {j in Ng..Ng}: x[j] = 5; # end condition

# DATA
data;
param Ng := 101;
param Dt := 0.1; # (10 - 0)/(Ng - 1)

# DOP.run - Simple problem to test dynamic optimization in AMPL format
# option solver bonmin;
solve;
display x;
display u;
