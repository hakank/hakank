/*

http://www.chlond.demon.co.uk/puzzles/puzzles2.html
"""
6. At a public-school camp five schools, Aldhouse, Bedminster, Chartry, Radford and Rugenham were represented. The smallest contingent from the five schools was greater than 20 but less than 30. Aldhouse sent two less than half of the Rugenham contingent. The Radford and Rugenham contingents together were 14 greater than the combined Bedminster and Chartry contingents. The Bedminster and Rugenham contingents together were two short of half the total complement from the five schools while the Chartry and Radford contingents combined mustered 13/32 of that total.

What was the strength of each contingent? (Clarke)    Solution
"""


! Description  : Public School Problem
! Source       : Clarke, L.H., (1954), Fun with Figures, William Heinemann Ltd.  
! Date written : Xpress-MP 25/10/99, Mosel 17/4/03
! Written by   : M J Chlond 


Answer:
Aldhouse             26
Bedminster           70
Chartry              38
Radford              66
Rugenham             56

Here:
x [*] :=
1  26
2  70
3  38
4  66
5  56
;

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

var x{1..5} integer;
var m >= 21 <= 29;

minimize any:
          x[1];

subject to fmin{i in 1..5}:
    x[i] >= m;

s.t.  cona: x[1]=.5*x[5]-2;
s.t.  conb: x[4]+x[5] = x[2]+x[3]+14;
s.t.  conc: x[2]+x[5] = sum{i in 1..5} 0.5*x[i]-2;
s.t.  cond: x[3]+x[4] = sum{i in 1..5} (13/32)*x[i];


option solver cplex;
solve;

display x;
display m;

