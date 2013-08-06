/*

  Puzzle in AMPL+CP.

  From Martin Henz' collection of puzzles
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/#curious
  """
  Curious Numbers from "Amusements in Mathematics, Dudeney", number 114.

  The number 48 has this peculiarity, that if you add 1 to it the result
  is a square number, and if you add 1 to its half, you also get a
  square number. Now, there is no limit to the numbers that have this
  peculiarity, and it is an interesting puzzle to find three more of
  them---the smallest possible numbers. What are they?
  """

  Here are some solutions on the form
  x, a,b,c,d,e:

    48, 49, 7, 24, 25, 5
    1680, 1681, 41, 840, 841, 29
    57120, 57121, 239, 28560, 28561, 169
    1940448, 1940449, 1393, 970224, 970225, 985


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param up; # upper range limit

var x >= 1 <= up integer;
var a >= 1 <= up integer;
var b >= 1 <= up integer;
var c >= 1 <= up integer;
var d >= 1 <= up integer;
var e >= 1 <= up integer;

minimize obj: x;

#
# constraints
#
s.t. c1:
  x + 1 = a  # if you add 1 to it
  and 
  a = b * b  # the result is a square number
  and 
  x = 2 * c  # if you to its half
  and 
  c + 1 = d  # add 1
  and 
  d = e * e  # you also get a square number
;

# s.t. c2: x < 48; # Test

# find larger numbers
s.t. c3: x > 48;
# s.t. c4: x > 1680;

# Note: Here we get integer overflow effects where both x and a = 1940450.
#       x should be 1940448 and a = x + 1 = 1940449
# s.t. c5: x > 57120; 
# s.t. c6: x > 1940448; # and of course here as well...

data;

param up := 4000000;


option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display x,a,b,c,d,e;
