/*
10,32,54,76,98
Xpress-Mosel Model

model 'onroad'

On my way to Philadelphia, I pass five mileposts that indicate their respective
distances to Philadelphia. The mileposts are at fixed intervals. What's curious i
s that each milepost has a two-digit number, and together the five mileposts use
all the digits from 0 to 9 once. What is the smallest distance that the closest m
ilepost can be from Philadelphia? Mileposts don't begin with 0. (Poniachek)



! Description  : On the road
! Source       : Poniachik, J. & L, (1998), Hard-to-solve Brainteasers, Sterling

! Date written : Xpress-MP 4/12/99, Mosel 19/4/03
! Written by   : M J Chlond

  uses 'mmxprs'

Xpress:
0 1 0 0 0 0 0 0 0 0 
1 0 0 0 0 0 0 0 0 0 
0 0 0 1 0 0 0 0 0 0 
0 0 1 0 0 0 0 0 0 0 
0 0 0 0 0 1 0 0 0 0 
0 0 0 0 1 0 0 0 0 0 
0 0 0 0 0 0 0 1 0 0 
0 0 0 0 0 0 1 0 0 0 
0 0 0 0 0 0 0 0 0 1 
0 0 0 0 0 0 0 0 1 0 

cplex;
(including the interpretation of the integers)
0 1 0 0 0 0 0 0 0 0     1
1 0 0 0 0 0 0 0 0 0     0
0 0 0 1 0 0 0 0 0 0     3 
0 0 1 0 0 0 0 0 0 0     2
0 0 0 0 0 1 0 0 0 0     5
0 0 0 0 1 0 0 0 0 0     4
0 0 0 0 0 0 0 1 0 0     7 
0 0 0 0 0 0 1 0 0 0     6
0 0 0 0 0 0 0 0 0 1     9
0 0 0 0 0 0 0 0 1 0     8

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/


set N := 1..10;
var x{N,N} binary; #  x(i,j) = 1 if digit (i-1) is in position j
var d;

minimize closest: 
        sum{i in N} 10*(i-1)*x[i,1] + sum{i in N} (i-1)*x[i,2];

s.t. dcon{i in N}:
        sum{j in N} x[i,j] = 1;

s.t. pcon{j in N}:
        sum{i in N} x[i,j] = 1;

s.t.  zero: x[1,1]+x[1,3]+x[1,5]+x[1,7]+x[1,9] = 0;

s.t.
  dista:
        sum{i in N} 10*(i-1)*x[i,1]+sum{i in N}(i-1)*x[i,2]+d =
        sum{i in N} 10*(i-1)*x[i,3]+sum{i in N}(i-1)*x[i,4];

s.t.
  distb:
        sum{i in N} 10*(i-1)*x[i,3]+sum{i in N}(i-1)*x[i,4]+d =
        sum{i in N} 10*(i-1)*x[i,5]+sum{i in N}(i-1)*x[i,6];

s.t.
  distc:
        sum{i in N} 10*(i-1)*x[i,5]+sum{i in N}(i-1)*x[i,6]+d =
        sum{i in N} 10*(i-1)*x[i,7]+sum{i in N}(i-1)*x[i,8];

s.t.
  distd:
        sum{i in N} 10*(i-1)*x[i,7]+sum{i in N}(i-1)*x[i,8]+d =
        sum{i in N} 10*(i-1)*x[i,9]+sum{i in N}(i-1)*x[i,10];

option solver cplex;
solve;

display d;
display x;

for{i in N} {
  for{j in N: x[i,j]>0} {
        printf "%d ", j-1;
  }
  printf "\n";
}

