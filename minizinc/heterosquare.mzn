%
% Heterosquare problem in Minzinc
%
% From http://willow.engr.uconn.edu/cometPubWiki/index.php/Heterosquare
% """
% A heterosquare of order n is a n*n square whose elements are distinct integers from 
% 1 to n^2 such that the sums of the rows, columns and diagonals are all different. 
% Here is an example of heterosquare of order 3 
% 
%            19
% 
% 1  2  3    6
% 8  9  4    21
% 7  6  5    18
% 
% 16 17 12   15  (Sums)
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; % for all_different

int: n = 3; % the size of the matrix

array[1..n, 1..n] of var 1..n*n: mat;   % the matrix
array[1..n] of  var 1..n*n*n: row_sums;
array[1..n] of  var 1..n*n*n: col_sums;
var 1..n*n*n: diag1;
var 1..n*n*n: diag2;

constraint 
   % all the entries in the matrix should be different
   all_different([ mat[i,j] | i,j in 1..n]) 

   /\ % and all sums should be different
   all_different(row_sums ++ col_sums ++ [diag1, diag2]) 

   /\ % calculate rows sums
   forall(i in 1..n) (
        sum(j in 1..n) (mat[i,j]) = row_sums[i]
   )
   /\ % calculate column sums
   forall(j in 1..n) (
       sum(i in 1..n) (mat[i,j]) = col_sums[j]
   )
   /\ % diag1 sums
   sum(i in 1..n) (mat[i,i]) = diag1

   /\ % diag2 sums
   sum(i in 1..n) (mat[i,n-i+1]) = diag2
;


%
% symmetry breaking
%
% From http://en.wikipedia.org/wiki/Heterosquare
% """
% It is strongly suspected that there are exactly 3120 
% essentially different heterosquares of order 3.
% """
%
% From
% http://en.wikipedia.org/wiki/Fr%C3%A9nicle_standard_form
% """
% A magic square is in Frénicle standard form, named for 
% Bernard Frénicle de Bessy, if the following two conditions apply:
%  - the element at position [1,1] (top left corner) is the smallest 
%    of the four corner elements; and
%  - the element at position [1,2] (top edge, second from left) is 
%    smaller than the element in [2,1].
% """
% (Note: For n=3 this gives 3120 solutions, as suspected...)
%
constraint
  minimum(mat[1,1], [mat[1,1], mat[1,n], mat[n,1], mat[n,n]])
  /\
  mat[1,2] < mat[2,1]
;


% solve satisfy;
solve :: int_search([ mat[i,j] | i,j in 1..n], first_fail, indomain_min, complete) satisfy;



output [
       "diag1: ", show(diag1), " diag2: ", show(diag2),"\n",
       "row_sums: ", show(row_sums), "\n",
       "col_sums: ", show(col_sums), "\n",

] ++ 
[ 
   show(mat[i,j]) ++ 
   if j mod n == 0 then "\n" 
   else " " endif 
   | i in  1..n, j in 1..n
] ++ 
[
  "mat: " ++ show(mat)
]
++
["\n"];
