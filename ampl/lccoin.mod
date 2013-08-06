/*
  Sat Jan  5 22:12:49 2008/hakank@bonetmail.com

After transaction coins are distributed as follows:
<br>
 Buyer has 1 crown and 1 threepence <br>
 Shopkeeper has 1 half-sovereign, 1 half-crown, 1 sixpence and 1 fourpence <br>
 Friend has 1 double-florin, one florin, 1 shilling and 1 penny
Xpress-Mosel Model

model 'lccoin'

! Description  : Lewis Carroll coin puzzle
! Source       : Wakeling, E., (1995), Rediscovered Lewis Carroll Puzzles, Dover.

! Date written : Xpress-MP 7/10/99, Mosel 17/4/03
! Written by   : M J Chlond

  uses 'mmxprs'

Xpress:
0 1 0 
1 0 0 
0 0 1 
0 1 0 
0 0 1 
0 0 1 
0 1 0 
0 1 0 
1 0 0 
0 0 1 

cplex (same solution)

0 1 0
1 0 0
0 0 1
0 0 1
0 1 0
0 1 0
0 0 1
0 1 0
1 0 0
0 0 1


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

param person := 3;
param coin := 10;

set P := 1..person; # buyer, shopkepper, friend
set C := 1..coin;
param requ{P};
param value{C};
var x{C,P} binary;

minimize any: x[1,1];

s.t. reqcon{j in P}:
        sum{i in C} value[i]*x[i,j] = requ[j];

s.t. useone{i in C}:
        sum{j in P} x[i,j] = 1;


data;

param  requ:= 
        1 63,
        2 160,
        3 85;
param value:= 
        1 120,
        2 60,
        3 48,
        4 30,
        5 24,
        6 12,
        7 6,
        8 4,
        9 3,
        10 1
;



#option presolve 0;
# option cplex_options "sensitivity";
option solver cplex;
#option solver bonmin;
#option solver cbc;
#option solver donlp2;
#option solver gjh;
#option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
#option solver LaGO;
#option solver loqo;
#option solver lpsolve;
#option solver minos;
#option solver pcx;
#option solver snopt;
#option solver umsip;

solve;

display x;

for{i in C} {
  for{j in P} {
        printf "%d ", x[i,j];
  }
  printf "\n";
}