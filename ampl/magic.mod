/* MAGIC, Magic Square */

   From GLPK magic.mod


 This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
 See also my AMPL page: http://www.hakank.org/ampl/

*/


/* Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru> */

/* In recreational mathematics, a magic square of order n is an
   arrangement of n^2 numbers, usually distinct integers, in a square,
   such that n numbers in all rows, all columns, and both diagonals sum
   to the same constant. A normal magic square contains the integers
   from 1 to n^2.

   (From Wikipedia, the free encyclopedia.) */

param n, integer, > 0, default 4;
/* square order */

set N := 1..n^2;
/* integers to be placed */

var x{i in 1..n, j in 1..n, k in N}, binary;
/* x[i,j,k] = 1 means that cell (i,j) contains integer k */

s.t. a{i in 1..n, j in 1..n}: sum{k in N} x[i,j,k] = 1;
/* each cell must be assigned exactly one integer */

s.t. b{k in N}: sum{i in 1..n, j in 1..n} x[i,j,k] = 1;
/* each integer must be assigned exactly to one cell */

var s;
/* the magic sum */

s.t. r{i in 1..n}: sum{j in 1..n, k in N} k * x[i,j,k] = s;
/* the sum in each row must be the magic sum */

s.t. c{j in 1..n}: sum{i in 1..n, k in N} k * x[i,j,k] = s;
/* the sum in each column must be the magic sum */

s.t. d: sum{i in 1..n, k in N} k * x[i,i,k] = s;
/* the sum in the diagonal must be the magic sum */

s.t. e: sum{i in 1..n, k in N} k * x[i,n-i+1,k] = s;
/* the sum in the co-diagonal must be the magic sum */


option solver cplex; 
# option solver bonmin; 
solve;

printf "\n";
printf "Magic sum is %d\n", s;
printf "\n";
for{i in 1..n}
{  printf{j in 1..n} "%3d", sum{k in N} k * x[i,j,k];
   printf "\n";
}
printf "\n";



end;
