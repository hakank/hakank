/*
Maximum of six watchmen can each see six cells. 

1  0  1  0
0  0  0  0
1  0  0  0
0  1  1  1

Maximum of six watchmen can each see seven cells. 

0  0  1  0
0  0  1  0
0  0  1  1
1  1  0  0	 

Xpress-Mosel Model

"""
Each watchman looks in all directions (horizontal, vertical and diagonal). 
On the board below, each watchman has five vacant cells under his gaze. A watchman can  see beyond another watchman.
What is the maximum number of watchmen that can be placed so that each sees six empty cells?

What if each watchman must see seven empty cells? (Poniachek)

"""

model 'evision'

! Description  : Equal Vision
! Source       : Poniachik, J. & L., (1998), Hard-to-solve Brainteasers, Sterling  
! Date written : Xpress-MP 19/12/99, Mosel 19/4/03
! Written by   : M J Chlond 

Xpress:
cvis=6
0 1 1 0 
1 0 0 1 
1 0 0 1 
0 0 0 0 

cvis=7
0 0 0 0 
1 1 1 0 
0 0 1 1 
0 1 0 0 

cplex (for cvis=6)
0 1 1 0
0 0 0 1
1 0 0 1
0 1 0 0

cplex for cvis=7
0 0 0 1
0 0 0 1
1 1 1 0
0 0 1 0

lpsolve cvis=6
0 0 0 0
1 0 0 1
1 0 0 1
0 1 1 0

lpsolve cvis=7
0 0 0 0
0 1 1 1
1 1 0 0
0 0 1 0

glpk cvis=6
0 1 1 0
1 0 0 0
1 0 0 0
0 1 1 0

glpk cvis=7
1 0 0 0
1 0 0 0
0 1 1 1
0 1 0 0


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

param size := 4;
param cvis := 7;

set S := 1..size;
set C := 1..cvis;
var x{S,S} binary; #  x(i,j) = 0 if cell {i,j} occupied, 1 otherwise
var n{S,S} integer <= 4*size-4 >= 0; # n(i,j) = number of vacant cells visible to watchman on cell {i,j}


# minimise vacant cells
minimize minv:
        sum{i in S,j in S} x[i,j];

s.t. nsee{i in S,j in S}:
        sum{m in S : m <> i and m-i+j >= 1 and m-i+j <= size} x[m,m-i+j]+
        sum{m in S : m <> i and i+j-m >= 1 and i+j-m <= size} x[m,i+j-m]+
        sum{m in S : m <> i} x[m,j] + sum{m in S : m <> j} x[i,m] = n[i,j];

s.t. sca{i in S,j in S}: n[i,j] >= cvis-99*x[i,j];
s.t. scb{i in S,j in S}: n[i,j] <= cvis+99*x[i,j];

option solver cplex;
# option solver lpsolve;


solve;
display x,n, _obj;

for{i in S} {
  for{j in S} {
      printf "%d ", 1-x[i,j];
  }
  printf "\n";
}

