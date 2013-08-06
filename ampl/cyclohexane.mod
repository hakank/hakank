/*
  Cyclohexane.

  Cf Choco's problem
 
  I steal the formulation from
  realpaver-0.4/benchmarks/cyclohexane

  realpaver gives the following

OUTER BOX 16
  x in [-10.85770359962651 , -10.85770359962645]
  y in [-0.7795480450791618 , -0.7795480450791532]
  z in [-0.77954804507916 , -0.7795480450791551]

  precision: 6.75e-14, elapsed time: 260 ms

minos:

18 iterations, objective -12.41679969
Nonlin evals: constrs = 50, Jac = 49.
x = -10.8577
y = -0.779548
z = -0.779548

Which I assume is correct.

Note: Choco gives the following two solutions:

2 solutions :
var nb 0 in [0.7795480427938404,0.7795480467625906]
var nb 1 in [10.857703593107566,10.857703604426678]
var nb 2 in [0.7795480434214537,0.7795480475384047]

var nb 0 in [0.7795480433100701,0.7795480478116362]
var nb 1 in [0.7795480426903287,0.7795480472569724]
var nb 2 in [10.857703592979153,10.857703610745348]

And Minos gives three different solutions if one maximizes
x, y, and z respectively.


   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/


*/

var x; # in  [-1.0e8,1.0e8] ,
var y; # in  [-1.0e8,1.0e8] ,
var z; #  [-1.0e8,1.0e8] ;

# There was no objective function in the realpaver model
maximize obj: x+y+z; 

# Constraints
c1:        13 + y^2*(1+z^2) + z*(z - 24*y)  = 0 ;
c2:        13 + z^2*(1+x^2) + x*(x - 24*z)  = 0 ;
c3:        13 + x^2*(1+y^2) + y*(y - 24*x)  = 0;


option solver minos;
solve;

display x,y,z;

