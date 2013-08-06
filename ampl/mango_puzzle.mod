/*

 ~/linear_programming/xpress/examples/puzzles/sol1s9.html.mos

Magno puzzle. Svaret ska bli 79 mangos.

Från
http://www.chlond.demon.co.uk/puzzles/puzzles1.html

"""
9. Three men who had a monkey bought a pile of mangoes. At night one of the men came to the pile of mangoes while the others slept and, finding that there was just one more mango than could be exactly divided by three, tossed the extra mango to the monkey and took away one third of the remainder. Then he went back to sleep.

Presently another of them awoke and went to the pile of mangoes. He also found just one too many to be divided by three so he tossed the extra one to the monkey, took one third of the remainder, and returned to sleep.

After a while the third rose also, and he too gave one mango to the monkey and took away the whole number of mangoes which represented precisely one third of the rest.

Next morning the men got up and went to the pile. Again they found just one too many, so they gave one to the monkey and divided the rest evenly. What is the least number of mangoes with which this can be done? (Kraitchik)    Solution 
"""


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

set INT = 1..5;
var x{INT} integer, >= 0;

# minval:= x(5)

s.t. take1:     
      -3*x[1]+x[2] = 1;
s.t. take2:
      -1.5*x[2]+x[3] = 1;
s.t. take3:
      -1.5*x[3]+x[4] = 1;
s.t. take4:
      -1.5*x[4]+x[5] = 1;

minimize m:
         x[5];


option solver cplex;

solve;

display x;
print "Lösningen: ", x[5];

