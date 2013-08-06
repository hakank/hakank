/*

   http://www.chlond.demon.co.uk/puzzles/puzzles1.html

   """
   2. Supposing that eleven coins with round holes are worth 15 bits, while 
   eleven square ones are worth 16 bits, and eleven of triangular shape are 
   worth 17 bits, tell how many round, square or triangular pieces of cash 
   would be required to purchase an item worth eleven bits. (Loyd)
   """

   From xpress/examples/puzzles/sol1s2.html.mos

   Answer: 7 coins with round holes, 1 coin with a square hole.

   Version 2:
   Testing to do it more talkative by using non-linearity
   (which glpk can't handle).

   x = 1.36364
   y = 1.45455
   z = 1.54545
   a = 7
   b = 1
   c = 0


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/


var x >= 0; # should not be integer
var y >= 0;
var z >= 0;
var a integer >= 0;
var b integer >= 0;
var c integer >= 0;

minimize zzz: a+b+c;

s.t.  
        c1: 11*x=15;
        c2: 11*y=16;
        c3: 11*z=17;

        c4: a*x + b*y + c*z = 11;
        


option solver cplex;
solve;


display x,y,z,a,b,c;
