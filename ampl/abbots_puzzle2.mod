/*

http://www.comp.nus.edu.sg/~henz/projects/puzzles/arith/index.html

The Abbot's Puzzle    from "Amusements in Mathematics, Dudeney", number 110.

If 100 bushels of corn were distributed among 100 people in such a
manner that each man received three bushels, each woman two, and each
child half a bushel, how many men, women, and children were there?

Dudeney added the condition that there are five times as many women as
men. That way, the solution becomes unique (otherwise, there are seven
solutions). 

ECLiPse:
[[5, 25, 70]]

AMPL:
Solution determined by presolve.
M = 5
W = 25
C = 70

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

var X{1..3} integer >= 1 <= 100;
#var M integer >= 1 <= 100; # X[1]
#var W integer >= 1 <= 100; # X[2]
#var C integer >= 1 <= 100; # X[3]

minimize m:
   sum{i in 1..3} X[i];


subject to con1:
        X[1] + X[2] + X[3] = 100;

# Men: 3, Women: 2, Children: 1/2 = 2*100 (i.e. multiply with 2)
# In AMPL there is no problem with 1/2.
subject to con2:
     X[1] * 3 + X[2] * 2 + X[3]/2 = 100; # no hack is needed in AMPL!
#    X[1] * 6 + X[2] * 4 + X[3] = 200; # though it's needed in ECLiPSe


# additional condition added by Dudeney      
subject to con4:
        X[1] * 5 = X[2];

option solver cplex;

solve;

display X;
