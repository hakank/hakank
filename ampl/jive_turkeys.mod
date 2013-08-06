/*

! Description  : Jive turkeys
! Source       : rec.puzzles
! Date written : Xpress-MP 25/10/99, Mosel 17/4/03
! Written by   : M J Chlond 

http://www.chlond.demon.co.uk/puzzles/puzzles2.html
"""
5. A butcher received an invoice for a consignment of 72 turkeys, but unfortunately it was smudged and a couple of figures were unreadable. All he could read was '-67.9-', with the first and last figures illegible. Nevertheless, being a 'rec.puzzler', he was able to work out the price of a turkey immediately. What was the price of a turkey? (rec.puzzles)
"""

Answer: £5.11

Here:
1    3
2    2
3  511


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/



*/

var x{1..3} integer >= 0;

minimize minprice:
         x[3]; 

s.t. const:
         10000*x[1] + 6790 + x[2] = 72*x[3];

s.t. intlimit1{i in 1..2}: x[i] <= 9;

s.t. intlimit2:
        x[3] <= 1345;

options solver cplex;
solve;

display x;
 