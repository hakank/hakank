/*
  Sat Jan  5 21:48:06 2008/hakank@bonetmail.com

Example:   1  0  0  1
           1  0  1  0
           1  1  0  0
           1  1  1  1
Xpress-Mosel Model

model 'evens'

"""
Take 16 coins and put them in four rows of four each. Remove 6 leaving an even n
umber of coins in each row and each column.(Kordemsky)
"""

! Description  : Evens puzzle
! Source       : Boris Kordemsky - The Moscow Puzzles (P8)
! Date written : MAGIC 26/11/92, Xpress-MP 22/6/98, Mosel 16/4/03
! Written by   : M J Chlond 

  uses 'mmxprs'

Xpress:
1 1 1 1 
1 0 0 1 
1 0 1 0 
1 1 0 0 


cplex:
0 1 0 1
1 1 1 1
0 1 1 0
1 1 0 0

bonmin:
0 1 1 0
1 1 1 1
0 0 1 1
1 0 1 0

lpsolve:
0 1 1 0
0 0 1 1
1 1 1 1
1 0 1 0


glpk:
0 1 1 0
1 0 1 0
0 0 1 1
1 1 1 1


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

param square := 4;
param coin := 10;
 
set S := 1..square;
var x{S,S} binary;
var n{S} integer;
var m{S} integer;

minimize any:
        x[1,1];
  
s.t. total: 
        sum{i in S,j in S} x[i,j] = coin;

s.t. rcon{i in S}:
        sum{j in S} x[i,j] = 2*n[i];

s.t. ccon{j in S}:
        sum{i in S} x[i,j] = 2*m[j];
    

# data;

#option presolve 0;
# för att skriva problem till en .lp-fil sätt writeprob=xxx.lp
#option cplex_options "sensitivity";
#option solver cplex;
# option solver bonmin;
#option solver cbc;
#option solver donlp2;
#option solver gjh;
#option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
#option solver LaGO;
#option solver loqo;
option solver lpsolve;
#option solver minos;
#option solver pcx;
#option solver snopt;
#option solver umsip;

solve;

display x;

for{i in S} {
    for{j in S}{
      printf "%d ", x[i,j];
    }
    printf "\n";
}
