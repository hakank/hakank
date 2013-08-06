/*
  Sat Jan  5 22:05:09 2008/hakank@bonetmail.com


            1  0  1  1  0
            0  1  1  1  0
            1  1  1  0  0
            1  1  0  1  1
            0  0  0  1  1

Xpress-Mosel Model

model 'five'

! Description  : 5 X 5 puzzle
! Source       : Unknown
! Date written : MAGIC 10/11/92, Xpress-MP 5/4/98, Mosel 17/4/03
! Written by   : M J Chlond

  uses 'mmxprs'

Xpress gives:
1 1 0 0 0 
1 1 0 1 1 
0 0 1 1 1 
0 1 1 1 0 
0 1 1 0 1 


cplex:

0 1 1 0 1
0 1 1 1 0
0 0 1 1 1
1 1 0 1 1
1 1 0 0 0

(Column reversed)

bonmin:
0 0 0 1 1
1 1 0 1 1
1 1 1 0 0
0 1 1 1 0
1 0 1 1 0

lpsolve:
1 0 1 1 0
0 1 1 1 0
1 1 1 0 0
1 1 0 1 1
0 0 0 1 1

cbc:
0 0 0 1 1
1 0 0 1 1
1 0 1 0 0
0 1 1 1 0
1 0 1 1 0

glpk:
1 1 0 0 0
1 1 0 1 1
0 0 1 1 1
0 1 1 1 0
0 1 1 0 1


   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/



*/

param n := 5;

set N := 1..n;
var x{N,N} binary;
var d{N,N} integer;

minimize moves: sum{i in N, j in N} x[i,j];

s.t. con{i in N,j in N}:
        sum{k in j-1..j+1 : k >= 1 and k <= n and k <> j} x[i,k] +
        sum{k in i-1..i+1 : k >= 1 and k <= n} x[k,j] = 2*d[i,j]+1;





# data;

#option presolve 0;
# för att skriva problem till en .lp-fil sätt writeprob=xxx.lp
#option cplex_options "sensitivity";
# option solver cplex;
# option solver bonmin;
# option solver cbc;
option solver gurobi;
#option solver donlp2;
#option solver gjh;
#option solver ipopt;
#option kestrel_options 'solver=xxx';
#option solver kestrel;
# option solver LaGO;
#option solver loqo;
# option solver lpsolve;
#option solver minos;
#option solver pcx;
#option solver snopt;
#option solver umsip;

solve;

display x;
display d;
display _obj;

for{i in N} {
   for{j in N} {
        printf "%d ", x[i,j];
   }
   printf "\n";
}


