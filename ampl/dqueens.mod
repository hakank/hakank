/*

  From Xpress-Mosel Model

  model 'dqueens'

  ! Description  : Dudeney's queen placement problem
  ! Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.  
  ! Date written : Xpress-MP 24/10/99, Mosel 17/4/03
  ! Written by   : M J Chlond 


Occupied squares have value 1:

0  0  0  0  0  0  0  0
0  0  0  0  0  0  0  0
0  0  0  0  0  0  0  0
0  0  0  0  0  0  0  0
0  0  0  0  0  0  0  0
0  0  0  0  0  0  1  1
0  0  1  0  0  0  1  1
0  0  1  1  0  0  0  1

Squares not under attack have value 0:

1  1  1  1  0  0  1  1
0  1  1  1  0  0  1  1
0  0  1  1  1  0  1  1
0  0  1  1  1  1  1  1
1  0  1  1  1  1  1  1
1  1  1  1  1  1  1  1
1  1  1  1  1  1  1  1
1  1  1  1  1  1  1  1


Express:
0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 
0 0 0 0 0 0 1 1 
0 0 1 0 0 0 1 1 
0 0 1 1 0 0 0 1 


cplex:
x:
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 1 1
0 0 1 0 0 0 1 1
0 0 1 1 0 0 0 1

a:
1 1 1 1 0 0 1 1
0 1 1 1 0 0 1 1
0 0 1 1 1 0 1 1
0 0 1 1 1 1 1 1
1 0 1 1 1 1 1 1
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 1


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/
  
param size := 8;

set S := 1..size;
var x{S,S} binary; # x(i,j) = 1 if square {I,J} occupied, 0 otherwise
var a{S,S} binary; # a(i,j) = 1 if square {I,J} attacked, 0 otherwise

# minimise number of squares attacked  
minimize attack: sum{i in S,j in S} a[i,j];

# all eight queens used
s.t. numq: sum{i in S,j in S} x[i,j] = 8;

# five of original queens untouched
s.t. sl:
        sum{j in 3..size} x[8,j] + x[7,size] + x[6,size] = 5;


# a(i,j) = 1 if square {i,j} attacked
s.t. att{i in S,j in S}:
        sum{m in S : m <> i and m-i+j >= 1 and m-i+j <= size} x[m,m-i+j]+
        sum{m in S : m <> i and i+j-m >= 1 and i+j-m <= size} x[m,i+j-m]+
        sum{m in S : m <> i} x[m,j] + 
        sum{n in S : n <> j} x[i,n] 
          <= 99*a[i,j];

option solver cplex;
# option solver cbc;

solve;

display _obj;

printf "x:\n";
for{i in S} {
  for{j in S} {
      printf "%d ", x[i,j];
  }
  printf "\n";
}


printf "a:\n";
for{i in S} {
  for{j in S} {
      printf "%d ", a[i,j];
  }
  printf "\n";
}
