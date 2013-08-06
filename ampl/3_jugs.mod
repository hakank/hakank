/*

3-jugs problem (as a shortest path problem)
Taha, Operations Research, page 245f 

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

/*

The model is based on the shortest_path_taha.mod
model from 
http://taha.ineg.uark.edu/shortest.txt
--------- shortest route model (Example 6.3-6)---------------- 

The result:

Shortest length from 1 to 15 = 7
Associated route:  1 -  9 - 10 - 11 - 12 - 13 - 14 - 15

*/
param n;   
param start;
param end;
param M=999999;
param d{i in 1..n, j in 1..n} default M;
param rhs{i in 1..n}=if i=start then 1
                     else (if i=end then -1 else 0);
     
var x{i in 1..n,j in 1..n:d[i,j]<M}>=0;
var outFlow{i in 1..n}=sum{j in 1..n:d[i,j]<M}x[i,j];
var inFlow{j in 1..n}=sum{i in 1..n:d[i,j]<M}x[i,j];
 
minimize z: sum{i in 1..n, j in 1..n:d[i,j]<M}d[i,j]*x[i,j]; 
subject to limit{i in 1..n}:outFlow[i]-inFlow[i]=rhs[i];

data;
param n:=15;
param start:=1;
param end:=15;


param d:
    1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 :=
 1  . 1 . . . . . . 1  .  .  .  .  .  .
 2  . . 1 . . . . . .  .  .  .  .  .  .
 3  . . . 1 . . . . 1  .  .  .  .  .  .
 4  . . . . 1 . . . .  .  .  .  .  .  .
 5  . . . . . 1 . . 1  .  .  .  .  .  .
 6  . . . . . . 1 . .  .  .  .  .  .  .
 7  . . . . . . . 1 1  .  .  .  .  .  .
 8  . . . . . . . . .  .  .  .  .  .  1
 9  . . . . . . . . .  1  .  .  .  .  .
10  . 1 . . . . . . .  .  1  .  .  .  .
11  . . . . . . . . .  .  .  1  .  .  .
12  . 1 . . . . . . .  .  .  .  1  .  .
13  . . . . . . . . .  .  .  .  .  1  .
14  . 1 . . . . . . .  .  .  .  .  .  1
15  . . . . . . . . .  .  .  .  .  .  .
;

option solver cplex;
# option solver lpsolve;



solve;
display x;
print "Shortest length from",start,"to",end,"=",z;
printf "Associated route: %2i",start;
for {i in 1..n-1} for {j in 2..n:d[i,j]<M} 
    {if x[i,j]=1 then printf" - %2i",j;} print;


