/*
http://www.chlond.demon.co.uk/puzzles/sol4s6.html

Xpress-Mosel Model

model 'nondomq'

! Description  : Non-dominating queens problem
! Source       : http://www.cli.di.unipi.it/~velucchi/queens.txt 
! Date written : Xpress-MP 8/4/2000, Mosel 19/4/03
! Written by   : M J Chlond 


Solution to board of order five:

Place 5 queens as follows:

     0   0   1   1   0
     1   0   0   1   0
     1   0   0   0   0
     0   0   0   0   0
     0   0   0   0   0

Attacked squares as follows:

     1   1   1   1   1
     1   1   1   1   1
     1   1   1   1   1
     1   1   1   1   0
     1   0   1   1   0

Leaving 3 squares not attacked.


Solutions up to and including board of order 17 are reproduced in a 
paper by Mario Velucchi to be found at http://anduin.eldar.org/~problemi/papers.html


AMPL-modell:

CPLEX 10.1.0: optimal integer solution; objective 22
2387 MIP simplex iterations
364 branch-and-bound nodes
numa = 22

x [*,*]
:   1   2   3   4   5    :=
1   0   1   1   0   0
2   0   1   0   0   1
3   0   0   0   0   1
4   0   0   0   0   0
5   0   0   0   0   0
;

a [*,*]
:   1   2   3   4   5    :=
1   1   1   1   1   1
2   1   1   1   1   1
3   1   1   1   1   1
4   0   1   1   1   1
5   0   1   1   0   1
;

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

param size = 5;
set S = 1..size;

var x{S,S} binary;  #  x(i,j) = 1 if square {i,j} occupied, 0 otherwise
var a{S,S} binary;  # a(i,j) = 1 if square {i,j} attacked, 0 otherwise
  
# minimise number of squares attacked or occupied
minimize numa:
        sum{i in S,j in S} a[i,j];

# number of pieces placed equals size of board
subject to nump:
        sum{i in S,j in S} x[i,j] = size;

# a[i,j) = 1 if square {i,j} attacked or occupied
subject to atta{i in S,j in S}:
        sum{m in S : m <> i and m-i+j >= 1 and m-i+j <= size} x[m,m-i+j] +
        sum{m in S : m <> i and i+j-m >= 1 and i+j-m <= size} x[m,i+j-m]+
        sum{m in S} x[m,j] + 
        sum{m in S : m <> j} x[i,m] +x[i,j]<= size*a[i,j] 
;

# a[i,j) = 0 if square {i,j} not attacked or occupied
subject to attb{i in S,j in S}:
        sum{m in S : m <> i and m-i+j >= 1 and m-i+j <= size} x[m,m-i+j]+
        sum{m in S : m <> i and i+j-m >= 1 and i+j-m <= size} x[m,i+j-m]+
        sum{m in S} x[m,j] + 
        sum{m in S : m <> j} x[i,m] +x[i,j] >= a[i,j] 
;        

option solver cplex;
solve;

display numa;
  
display x;
display a;
