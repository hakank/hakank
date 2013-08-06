/*

  Dudeney numbers in AMPL+CP.

  Dudeney numbers
  From Pierre Schaus blog post
  Dudeney number
  http://cp-is-fun.blogspot.com/2010/09/test-python.html
  """
  I discovered yesterday Dudeney Numbers
  A Dudeney Numbers is a positive integer that is a perfect cube such that the sum
  of its decimal digits is equal to the cube root of the number. There are only six
  Dudeney Numbers and those are very easy to find with CP.
  I made my first experience with google cp solver so find these numbers (model below)
  and must say that I found it very convenient to build CP models in python!
  When you take a close look at the line:
      solver.Add(sum([10**(n-i-1)*x[i] for i in range(n)]) == nb)
  It is difficult to argue that it is very far from dedicated
  optimization languages!
  """
  
  Also see: http://en.wikipedia.org/wiki/Dudeney_number
  
  There are 6 solutions:
      1, 512, 4913, 5832, 17576, 19683


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

# decision variables
var x{1..n} >= 0 <= 9 integer;
var nb >= 1 <= 10^n integer;
var s >= 1 <= 9*n+1 integer;

minimize obj: nb;

#
# constraints
#
s.t. c1: nb = s*s*s;
s.t. c2: s = sum{i in 1..n} x[i];
s.t. c3: nb = sum{i in 1..n} ceil(10^(n-i))*x[i];

# Get the 6 different solutions
s.t. c4: nb > 1;
# s.t. c5: nb > 512;
# s.t. c6: nb > 4913;
# s.t. c7: nb > 5832;
# s.t. c8: nb > 17576;
# s.t. c9: nb > 19683;


data;

param n := 6;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display x, nb, s;



