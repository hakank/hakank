/*
  
  Remainder_puzzle in AMPL+CP.

  From
  http://www.chlond.demon.co.uk/puzzles/puzzles1.html
  """
  10. Is there a number which when divided by 3 gives a remainder of 1; when divided by 4, 
  gives a remainder of 2; when divided by 5, gives a remainder of 3; and when divided by 6, 
  gives a remainder of 4? (Kordemsky)
  """

  Answer: 58.

  This is a more general solution than
    http://www.hakank.org/ampl/remainder_puzzle.mod
 

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/
   
*/

param n;

# decision variables
var x{1..n} >= 1 integer;
var z >= 1 integer;     

s.t. c1{i in 1..n}: -(i+2)*x[i] + z  = i;

data;

param n := 4;

option solver gecode;
option gecode_options 'var_branching=size_min val_branching=min outlev=1 outfreq=1';

# option solver cplex;

solve;

display x, z;
