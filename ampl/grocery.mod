/*

  Grocery puzzle in AMPL+CP.

  Problem from: Christian Schulte, Gert Smolka, Finite Domain
  "Constraint Programming in Oz. A Tutorial". 2001.
  http://www.mozart-oz.org/documentation/fdt/
  """
  A kid goes into a grocery store and buys four items. The cashier
  charges $7.11, the kid pays and is about to leave when the cashier
  calls the kid back, and says "Hold on, I multiplied the four items
  instead of adding them; I'll try again; Hah, with adding them the
  price still comes to $7.11''. What were the prices of the four items?
  """

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/


param S := 711;
var A >= 1 integer <= 711 ;
var B >= 1 integer <= 711 ;
var C >= 1 integer <= 711 ;
var D >= 1, integer <= 711 ;


s.t. c1: A + B + C + D = S;
s.t. c2: A * B * C * D =  S * 100 * 100 * 100;
s.t. c3: 
   A < B and 
   B < C and  
   C < D
;

## Extra constraint for efficiency
# s.t. c4: D mod 79 = 0;

option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";


solve;

display {A,B,C,D};
display A+B+C+D;
display {A/100, B/100, C/100, D/100};
