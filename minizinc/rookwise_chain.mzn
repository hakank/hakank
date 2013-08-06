% 
% Rookwise chain puzzle in MiniZinc.
% 
% Martin Gardner (June 1961)
% """   
% [P]lace the digits [1..9] on a 3 x 3 matrix so that they form a rookwise
% connected chain, from 1 to 9, and also in such a way that the bottom row
% is the sum of the first two rows. The answer is unique.
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 3;
array[1..n, 1..n] of var 1..n*n: x;
array[1..n] of var int: row_sums; % sum of the rows

predicate toNum10(array[int] of var int: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n] ++ row_sums, first_fail, indomain_min, complete) satisfy;


constraint
   forall(i in 1..n) ( 
      row_sums[i] > 0
   )
   /\ 
   all_different([x[i,j] | i,j in 1..n])
   /\ % sum of rows
   forall(i in 1..n) (
      toNum10([x[i,j] | j in 1..n], row_sums[i])
   )
   /\ % sum of first 1..n-1 rows = n'th row
   sum(i in 1..n-1) (row_sums[i]) = row_sums[n]

   /\ % the rook moves
   forall(k in 1..n*n-1) (
     exists(i, j in 1..n) (
        k = x[i, j] % fix this k
        /\
        exists(a, b in {-1, 0, 1}
          where 
          i+a >= 1 /\ j+b >= 1 
          /\
          i+a <= n /\ j+b <= n
          /\ 
          abs(a+b) = 1 % just move in exactly one direction
        ) 
        (
          % find the next k
          k + 1 = x[i+a, j+b]  
        )
     )
  )
;


output 
[
  "\nrow_sums: ", show(row_sums)
] ++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i, j in 1..n
] ++ ["\n"]
;