/*

  Cyclohexane problem in AMPL+CP.
  (Circling the Squares puzzle)

  From the Oz examples
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/circlingsquares.html
  """
  from "Amusements in Mathematics, Dudeney", number 43.
  The puzzle is to place a different number in each of the ten squares
  so that the sum of the squares of any two adjacent numbers shall be
  equal to the sum of the squares of the two numbers diametrically
  opposite to them. The four numbers placed, as examples, must stand as
  they are. Fractions are not allowed, and no number need contain more
  than two figures.
  """

  There are 6 solutions to this problem:
    16, 2, 19, 47, 26, 8, 14, 13, 49, 22
    16, 2, 19, 22, 49, 8, 14, 13, 26, 47
    16, 2, 26, 47, 19, 8, 14, 22, 49, 13
    16, 2, 26, 13, 49, 8, 14, 22, 19, 47
    16, 2, 49, 13, 26, 8, 14, 47, 19, 22
    16, 2, 49, 22, 19, 8, 14, 47, 26, 13


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
set letters;

# decision variables
var x{letters} >= 1 <= 99 integer;

#
# constraints
#
s.t. c1: alldiff{i in letters} x[i];

s.t. c2: 
   x['A'] = 16 and
   x['B'] = 2 and
   x['F'] = 8 and
   x['G'] = 14
;

s.t. c3:
   x['A']*x['A'] + x['B']*x['B'] = x['F']*x['F'] + x['G']*x['G'] and
   x['B']*x['B'] + x['C']*x['C'] = x['G']*x['G'] + x['H']*x['H'] and
   x['C']*x['C'] + x['D']*x['D'] = x['H']*x['H'] + x['I']*x['I'] and
   x['D']*x['D'] + x['E']*x['E'] = x['I']*x['I'] + x['K']*x['K'] and
   x['E']*x['E'] + x['F']*x['F'] = x['K']*x['K'] + x['A']*x['A']
;


data;

param n := 10;
set letters = A B C D E F G H I K;


# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display x;

