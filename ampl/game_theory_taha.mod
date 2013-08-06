/*
  Tue Jul  8 13:13:51 2008/hakank@bonetmail.com

  Game theory (2 player zero sum game)
  from Taha Operations Research (8'th edition), page 528.

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

set I;
set J;
param M {I,J};      # payoff matrix
var X{J} >= 0;    # probability of strategy j for column player
var v;

maximize game:  v;
subject to constraints {i in I}:
        v - sum {j in J} M[j,i]*X[j] <=0;

subject to probability:
        sum {j in J} X[j]=1;


data;

set I := 1 2 3 ;
set J := 1 2 3 ;
param M:
	1	2	3 :=	#left hand side of constraint equations
 1	3	-1	-3	#A[i,j] i row label, j column label
 2	-2	4	-1
 3	-5	-6	2 ;


#option presolve 0;
# för att skriva problem till en .lp-fil sätt writeprob=xxx.lp
# option cplex_options "sensitivity";
option solver cplex;

solve;

display v;
display X;
