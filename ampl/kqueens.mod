/*
0  0  1  0  0  0  0  0
0  0  0  0  0  0  1  0
0  0  0  0  0  0  0  0
0  0  0  0  0  0  0  0
0  1  0  0  0  0  0  0
0  0  0  0  0  1  0  0
0  0  0  0  0  0  0  1
0  0  0  0  0  0  0  0

Xpress-Mosel Model

model 'kqueens'

! Description  : Kraitchik's queen placement problem
! Source       : Kraitchik, M., (1942), Mathematical Recreations, W.W. Norton and Company, Inc.    
! Date written : Xpress-MP 26/10/99, Mosel 17/4/03
! Written by   : M J Chlond 

Xpress:
1 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 
0 0 0 0 1 0 0 0 
0 1 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 1 
1 0 0 0 0 0 0 0 

cplex:
x:
0 1 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 1 0 0
0 0 0 0 1 0 0 0
0 0 0 0 1 0 0 0
0 0 0 0 0 0 0 0
1 0 0 0 0 0 0 0

a:
1 0 1 1 1 1 1 1
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1
1 1 1 1 1 0 1 1
1 1 1 1 0 1 1 1
1 1 1 1 0 1 1 1
1 1 1 1 1 1 1 1
0 1 1 1 1 1 1 1

glpk:
Time used:   89.0 secs
Memory used: 12.8 Mb (13426923 bytes)
x:
0 0 1 0 0 0 0 0
0 0 0 0 0 1 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 1 0 0 0 0 0 0
0 0 0 0 1 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 1

a:
1 1 0 1 1 1 1 1
1 1 1 1 1 0 1 1
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1
1 0 1 1 1 1 1 1
1 1 1 1 0 1 1 1
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 0
)


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/



*/
  
param size := 8;

set S := 1..size;
var x{S,S} binary;  # x(i,j) = 1 if square {I,J} occupied, 0 otherwise
var a{S,S} binary;  # a(i,j) = 1 if square {I,J} attacked, 0 otherwise

  
# minimise number of queens
minimize numq: sum{i in S,j in S} x[i,j];

# ! a[i,j] = 0 if square {i,j} not attacked
s.t. att{i in S,j in S}:
        sum{m in S : m <> i and m-i+j >= 1 and m-i+j <= size} x[m,m-i+j]+
        sum{m in S : m <> i and i+j-m >= 1 and i+j-m <= size} x[m,i+j-m]+
        sum{m in S} x[m,j] + 
        sum{n in S : n <> j} x[i,n] >= a[i,j];

# each square either attacked or occupied
s.t. mb{i in S,j in S}:
        a[i,j]+x[i,j] = 1;
  
option solver cplex;
solve;

# display _obj;
printf "numq: ";
printf (sum{i in S,j in S} x[i,j]);
printf "\n";

printf "x:\n";
for{i in S} {
  for{j in S} {
     printf "%d ", x[i,j];
  } 
  printf "\n";
}

printf "\na:\n";
for{i in S} {
  for{j in S} {
     printf "%d ", a[i,j];
  } 
  printf "\n";
}
