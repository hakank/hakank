/*

  General store problem in AMPL+CP.

  From
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
  """
  The General Store  from "Mathematical Puzzles of Sam Loyd", number 30

  The owner of a general store, who is something of a puzzlist, has put
  up this sign to see if any of his mathematical friends can translate
  it properly. Each different letter stands for a different digit. The
  words above the horizontal line represent numbers that add to the
  total of "ALL WOOL". The problem is to change all the letters to the
  correct digits.

         C H E S S
   +       C A S H
   +   B O W W O W
   +     C H O P S
   +   A L S O P S
   + P A L E A L E
   +       C O O L
   +       B A S S
   +       H O P S
   +       A L E S
   +       H O E S
   +   A P P L E S
   +       C O W S 
   +   C H E E S E
   +   C H S O A P
   +     S H E E P
   _______________
     A L L W O O L
   """

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

# decision variables
var C >= 0 <= 9 integer;
var H >= 0 <= 9 integer;
var E >= 0 <= 9 integer;
var S >= 0 <= 9 integer;
var A >= 0 <= 9 integer;
var B >= 0 <= 9 integer;
var O >= 0 <= 9 integer;
var W >= 0 <= 9 integer;
var P >= 0 <= 9 integer;
var L >= 0 <= 9 integer;


var x{1..n} >= 0 <= 9 integer;

#
# constraints
#
s.t. c0: 
   x[1] = C and
   x[2] = H and
   x[3] = E and
   x[4] = S and
   x[5] = A and
   x[6] = B and
   x[7] = O and
   x[8] = W and
   x[9] = P and 
   x[10] = L
;
s.t. c1: alldiff{i in 1..n}  x[i];

s.t. c2:

                            10000*C + 1000*H + 100*E + 10*S + S
   +                                  1000*C + 100*A + 10*S + H
   +             100000*B + 10000*O + 1000*W + 100*W + 10*O + W
   +                        10000*C + 1000*H + 100*O + 10*P + S
   +             100000*A + 10000*L + 1000*S + 100*O + 10*P + S
   + 1000000*P + 100000*A + 10000*L + 1000*E + 100*A + 10*L + E
   +                                  1000*C + 100*O + 10*O + L
   +                                  1000*B + 100*A + 10*S + S
   +                                  1000*H + 100*O + 10*P + S
   +                                  1000*A + 100*L + 10*E + S
   +                                  1000*H + 100*O + 10*E + S
   +             100000*A + 10000*P + 1000*P + 100*L + 10*E + S
   +                                  1000*C + 100*O + 10*W + S
   +             100000*C + 10000*H + 1000*E + 100*E + 10*S + E
   +             100000*C + 10000*H + 1000*S + 100*O + 10*A + P
   +                        10000*S + 1000*H + 100*E + 10*E + P   
   = 1000000*A + 100000*L + 10000*L + 1000*W + 100*O + 10*O + L
;

data;

param n := 10;

# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=degree_max val_branching=max outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=auto alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display x;



