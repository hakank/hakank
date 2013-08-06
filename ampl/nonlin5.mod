/*
  From http://www.numerical.rl.ac.uk/lancelot/sif/node7.html


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/
option randseed 0;
param n := 100;
var y;
# randomize the values to not fall into local minimum
var x{1..n} >= -1 <= 1 default Normal(0,1); # Uniform01();

display x;

minimize z:
        1/2 *(( x[1] - x[n])*x[2] + y)^2 + 2*x[1]^2 + 2*x[1]*x[n];


s.t.
        c1{i in 1..n-1}: x[1]*x[i+1] + (1+2/i)* x[i]*x[n] + y <= 0;

        c2{i in 1..n}: sin(x[i])^2 <= 1/2;

        c3: (x[1] + x[n])^2 = 1;


# option solver bonmin;
# option solver donlp2;
# option solver snopt;
solve;

display x, y;
