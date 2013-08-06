/*

! Description  : Tommy's Birthday Coins
! Source       : Clarke, L.H., (1954), Fun with Figures, William Heinemann Ltd.  
! Date written : Xpress-MP 25/10/99, Mosel 17/4/03
! Written by   : M J Chlond 

http://www.chlond.demon.co.uk/puzzles/puzzles2.html
"""
2. Tommy was given 15 coins for his birthday, all in half-crowns, shillings and sixpences. When he added it up he found that he had 1 5s. 6d. How many half-crowns was he given? (Clarke)    Solution 
"""

Answer: 8 half-crowns, 4 shillings and 3 sixpences

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/


param coin = 3;

set C = 1..coin;
param array{C};
var x{C} integer >= 1;
param value{C};

minimize any:  
         x[1];

subject to tval:
         sum{i in C} value[i]*x[i] = 306;

subject to tnum:
         sum{i in C} x[i] = 15;

data;

param value:= 
      1 30
      2 12
      3 6;

option solver cplex;
solve;

display x;
