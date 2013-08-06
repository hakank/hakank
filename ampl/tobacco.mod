/*

! Description  : Clarke's tobacconist
! Source       : Clarke, L.H., (1954), Fun with Figures, William Heinemann Ltd.
! Date written : Xpress-MP 25/10/99, Mosel 17/4/03
! Written by   : M J Chlond 

http://www.chlond.demon.co.uk/puzzles/puzzles2.html
"""
1. A tobacconist bought a quantity of pipes at 2s. 1d. each and others at 4s. 1d. each. 
   He spent in all 8 6s. 8d. on the pipes. How many of each kind did he buy? (Clarke)    Solution 
"""

Solution: 31 at 2s. 1d. and 25 at 4s. 1d 

Here:
x [*] :=
1  31
2  25
;

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

# x[1]: 
var x{1..2} integer >= 0;

subject to con:
        25*x[1]+49*x[2] = 2000;


minimize any:
         x[1];


option solver cplex;
# option solver cbc;
#option solver bonmin;
solve;

display x;
