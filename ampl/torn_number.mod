/*

  Torn number puzzle in AMPL+CP.

  Problem statement and model inspiration from
  """
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/torn.html
  The Torn Number    from "Amusements in Mathematics, Dudeney", number 113
  
  I had the other day in my possession a label bearing the number 3025
  in large figures. This got accidentally torn in half, so that 30 was
  on one piece and 25 on the other. On looking at these pieces I began
  to make a calculation, scarcely concious of what I was doing, when I
  discovered this little peculiarity. If we add the 30 and the 25
  together and square the sum we get as the result the complete original
  number on the label! Now, the puzzle is to find another number,
  composed of four figures, all different, which may be divided in the
  middle and produce the same result.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

# decision variables
var D0 >= 0 <= 9 integer;
var D1 >= 0 <= 9 integer;
var D2 >= 0 <= 9 integer;
var D3 >= 0 <= 9 integer;
var Sum >= 0 integer;

var x{1..4} >= 0 <= 9 integer;

#
# constraints
#
s.t. c1:
   D3 != 0 and
   Sum = D3 * 10 + D2 + D1 * 10 + D0 and
   Sum * Sum = D3 * 1000 + D2 * 100 + D1 * 10 + D0 and
   not (30 = D3*10 + D2  or 30 = D1*10 + D0) and # none of the bits a30
   not (25 = D3*10 + D2  or 25 = D1*10 + D0)  # none of the bits are 25
;

s.t. c2: x[1] = D0 and x[2] = D1 and x[3] = D2 and x[4] = D3;

s.t. c2: alldiff{i in 1..n} x[i];



data;


option show_stats 2;
# option presolve 0;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display D0, D1, D2, D3, Sum;