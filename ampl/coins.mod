/*

   http://www.chlond.demon.co.uk/puzzles/puzzles1.html 
   """
   2. Supposing that eleven coins with round holes are worth 15 bits, 
   while eleven square ones are worth 16 bits, and eleven of triangular 
   shape are worth 17 bits, tell how many round, square or triangular 
   pieces of cash would be required to purchase an item worth eleven 
   bits. (Loyd)
   """

   xpress/examples/puzzles/sol1s2.html.mos

   Answer:  7 coins with round holes, 1 coin with a square hole.

   I changed from 1..3 to the verbose version.

   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/

*/

# param denom = 3;

# set D = 1..denom;
set D = {"round", "square", "triangular"};
# 1 round  (answer: 7 )
# 2 square (answer: 1)
# 3 triangular (answer: 0)

param v{D}; 

var x{D} integer, >= 0;

param requ:= 121; # i.e. 11*11

data;
param v := 
      round 15 
      square 16 
      triangular 17;

subject to vcon:
        sum{i in D} v[i]*x[i] = requ;

minimize ncoins:
         sum{i in D} x[i]
         ;

option solver cplex;
solve;

display 11*15, 11*16, 11*17, 11*11;
display x;
