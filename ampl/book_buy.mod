/*
http://www.chlond.demon.co.uk/puzzles/sol4s9.html

AMPL Model

# bookbuy.mod
# AMPL coding: M.J.Chlond, April 2000.
#
# Source:  M Kraitchik, Mathematical Recreations(p37), Dover
#
# Four men, Peter and Paul and their sons Tom and Dick, buy books. When their 
# purchases are completed it turns out that each man has paid for each of his 
# books a number of dollars equal to the number of books he has bought. Each
# family (father and son) has spent $65. Peter has bought one more book than
# Tom, and Dick has bought only one book. Who is Dick's father?
#

Answer:
Peter is Dick's father.

Note: This was an AMPL model from Chlond, so this is not an translation.

Here:
CPLEX 10.1.0: Constraint _scon[1] is not convex quadratic since it is an equality constraint.
]
w = 0

:   x   y    :=
1   0   0
2   0   1


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

# fathers: 1 = Peter, 2 = Paul, sons: 1 = Tom, 2 = Dick
param m := 2;   
set M := 1..m;

# w = 1 if Peter is Tom's father, 0 otherwise 
var w;           
# number of books (and price) bought by father i
var x {i in M}, >= 1, <=8, integer; 
# number of books (and price) bought by son j
var y {i in M}, >= 1, <=8, integer; 

fix y[2] := 1;   # Dick buys one book

minimize any: x[1];

# Peter buys one more book than Tom
subject to
        pcon: x[1] = y[1]+1;                      
 
 # each family spends $65
        fcona: x[1]^2 + w*y[1]^2 + (1-w)*y[2]^2 = 65;
        fconb: x[2]^2 + (1-w)*y[1]^2 + w*y[2]^2 = 65;    

# option solver cplex;
# option solver loqo;
# option solve snopt;
# option solver bonmin;

# Connects to the Kestrel server:
option kestrel_options 'solver=minlp';
#option kestrel_options 'solver=MOSEK';
option solver kestrel;

solve;
display any, w, x, y;
