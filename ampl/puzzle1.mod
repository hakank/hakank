/*
  http://www.thescripts.com/forum/thread42216.html

The puzzle: a 4 x 4 grid. The rows are summed (and given), the
cols are summed (and given), and the two diagonals are summed,
and given. In addition, 4 clues are given, but none of the 4 are in
the same row or col.

Example from today's paper:...solution time is 8 minutes, 1 second,
so they say.

The set of allowable numbers is 1 thru 9

Rows:
3 + B + C + D = 22
E + F + 8 + H = 26
I + J + K + 8 = 31
M + 7 + O + P = 25

Col sums:
24, 18, 31, 31

Diag sums:
3 + F + K + P = 24
M + J + 8 + D = 24

Svar:

cplex:
 3  3  8  8
 9  3  8  6
 9  5  9  8
 3  7  6  9

lpsolve:
 3  3  9  7
 8  3  8  7
 9  5  9  8
 4  7  5  9

(Note: The MiniZinc model gives 16 different solution for this
 problem. http://www.hakank.org/minizinc/puzzle1.mzn)


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

param n := 4;
param rowsums{1..n};
param colsums{1..n};
param diagsums{1..n/2};
param matrix{1..n, 1..n} default 0;

var x{1..n, 1..n} integer >= 1 <= 9;


maximize z: x[1,2];

makematrix{i in 1..n, j in 1..n: matrix[i,j] > 0}: matrix[i,j] = x[i,j];
rowsum{i in 1..n}: sum{j in 1..n} x[i,j] = rowsums[i];
colsum{j in 1..n}: sum{i in 1..n} x[i,j] = colsums[j];
diag1: sum{i in 1..n} x[i,i] = diagsums[1];
diag2: sum{i in 1..n} x[i,n-i+1] = diagsums[2];

#/*
data;

param rowsums := 
        1 22
        2 26
        3 31
        4 25
;

param colsums := 
        1 24
        2 18
        3 31
        4 31
;

param diagsums :=
        1 24
        2 24
;

# clues
param matrix:  1 2 3 4 :=
1  5 . . .
2  . . 8 .
3  . . . 8
4  . 7 . .
;
#*/


option solver cplex;
# option solver bonmin;
#option solver lpsolve;
solve;

display x;
display rowsums, colsums, diagsums;

for {i in 1..n} {
  for {j in 1..n} {
    printf "%3d ", x[i,j];
  }

  printf " = %d\n", sum{k in 1..n} x[i,k];
}

for {j in 1..n} {
  printf "%3d ", sum{k in 1..n} x[k,j]; 
}
printf "\n";

printf "diag1: %d\n",sum{i in 1..n} x[i,i];
printf "diag2: %d\n", sum{i in 1..n} x[i,n-i+1];



/*
data;

param rowsums := 
        1 22
        2 26
        3 31
        4 25
;

param colsums := 
        1 24
        2 18
        3 31
        4 31
;

param diagsums :=
        1 24
        2 24
;
*/
