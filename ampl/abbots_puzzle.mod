/*

http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.
html
The
Abbot's Puzzle    from "Amusements in Mathematics, Dudeney", number 110.

If 100 bushels of corn were distributed among 100 people in such a
manner that each man received three bushels, each woman two, and each
child half a bushel, how many men, women, and children were there?

Dudeney added the condition that there are five times as many women as
men. That way, the solution becomes unique (otherwise, there are seven
solutions). 

ECLiPSe:
[[5, 25, 70]]

AMPL:
Solution determined by presolve.
M = 5
W = 25
C = 70

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

var M integer >= 1 <= 100;
var W integer >= 1 <= 100;
var C integer >= 1 <= 100;

subject to con1:
        M + W + C = 100;

# Men: 3, Women: 2, Children: 1/2 = 2*100 (i.e. multiply with 2)
subject to con3:
        M * 6 + W * 4 + C = 200;

# additional condition added by Dudeney      
subject to con4:
        M * 5 = W;

option solver cplex;

solve;

display M,W,C;


  