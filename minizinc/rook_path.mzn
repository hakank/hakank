% 
% Rooks path problem in MiniZinc.
% 
% Find a rook path on a n x n matrix.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 3;
array[1..n, 1..n] of var 1..n*n: x;

% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n], first_fail, indomain_max, complete) satisfy;



constraint
  all_different([x[i,j] | i,j in 1..n])
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
          abs(a+b) = 1
        ) 
        (
          % find the next k
          k + 1 = x[i+a, j+b]  
        )
     )
  )

;

output[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i, j in 1..n
] ++ ["\n"]
;