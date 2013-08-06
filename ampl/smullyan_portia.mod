/*
   A Smullyan problem

From http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
Katta G. Murty: "Optimization Models for Decision Making"

Smullyan 1978 är "What is the name of this book".

"""
The setting of this problem from [R. M. Smullyan, 1978] is William
Shakespeare.s play .The Merchant of Venice. written in the year 1597.
In this play, a girl named Portia is the lead female character. She was
a law graduate with an obsession for highly intelligent boys. Her sole
concern was with .intelligence., completely ignoring other character-
istics that girls usually associate with desirable life-mates. Her life.s
ambition was to marry an extremely intelligent boy, no matter how he
looks or behaves, or how wealthy he is. For achieving this goal she
devised a very clever scheme to choose her fiance.
She purchased three caskets, one of gold, silver, and lead, and hid
a stunningly beautiful portrait of herself in one of them. The suitor
was asked to identify the casket containing the portrait. If his choice is
correct, he can claim Portia as his bride; otherwise he will be perma-
nently banished to guarantee that he won.t appear for the test again.
To help the suitor choose intelligently, Portia put inscriptions on the
caskets as in Figure 7.1. And she explained that at most one of the
three inscriptions was true. She reasoned that only an intelligent boy
could identify the casket containing the portrait with these clues.

The portrait           The portrait        The portrait is
is in this casket      is not in this      not in the gold
                       casket              casket
  
1=Gold                 2=Silver             3=Lead
"""


cplex:
x1 = 0
x2 = 1
x3 = 0

y1 = 0
y2 = 0
y3 = 1

i.e.:
  x2 = 1 -> painting in Silver
  y3 = 1 -> third sentence is true

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

# xj: 1 if the jth casket contains the portrait, 0 otherwise
# yj: 1 if the incription of the jth casket is true, 0 otherwise
var x1 binary;
var x2 binary;
var x3 binary;

var y1 binary;
var y2 binary;
var y3 binary;

s.t. 
   c1: x1 + x2 + x3 = 1;  # exactly one casket has the portrait
   c2: -x1 + y1 = 0;      # Gold: the portrait is in this casket
   c3: x2 + y2 = 1;       # Silver: the p. is not in this casket 
   c4: x1 + y3 = 1;       # Lead: the p. is not in the gold casket
   c5: y1 + y2 + y3 <= 1; # at most one description is true 
   


option solver cplex;
# option solver lpsolve;

solve;

display x1,x2,x3;
display y1,y2,y3;



