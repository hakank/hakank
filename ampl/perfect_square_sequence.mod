/*

  Perfect square sequence in AMPL+CP.

  From "Fun with num3ers"
  "Sequence"
  http://benvitale-funwithnum3ers.blogspot.com/2010/11/sequence.html
  """
  If we take the numbers from 1 to 15 
      (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) 
  and rearrange them in such an order that any two consecutive 
  numbers in the sequence add up to a perfect square, we get,
  
  8     1     15     10     6     3     13     12      4      5     11     14        2      7      9
      9    16    25     16     9     16     25     16     9     16     25     16       9     16
  
  
  I ask the readers the following:
  
  Can you take the numbers from 1 to 25 to produce such an arrangement?
  How about the numbers from 1 to 100?
  """
  
  Via http://wildaboutmath.com/2010/11/26/wild-about-math-bloggers-111910
  

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;

set squares = setof{i in 1..n} i^2;

# decision variables
var x{1..n} >= 1 <= n integer;

#
# constraints
#
s.t. c1: alldiff{i in 1..n} x[i];

# the addition of pairs must be a square
s.t. c2{i in 2..n}: 
     exists{z in 1..2*n}
       z*z = (x[i-1]+x[i])
;

# symmetry breaking
s.t. c3: x[1] < x[n];

data;

# param n := 15;
param n := 25;
# param n := 100;

option show_stats 2;

display squares;

option solver gecode;
# option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";
option gecode_options "var_branching=size_min val_branching=split_min outlev=1 outfreq=1";


# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display x;

