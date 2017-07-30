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
int: n = 7;
array[1..n, 1..n] of var 1..n*n: x;

predicate next(array[int,int] of var int: a, 
               var int: k1,
               var int: k2) =

     exists(i, j in 1..n) (
        k1 = x[i, j] % fix this k
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
          k2 = x[i+a, j+b]  
        )
     )

;

% solve satisfy;
solve :: int_search(
        [x[i,j] | i,j in 1..n], 
        max_regret, % first_fail, 
        indomain_split, 
        complete) satisfy;



constraint
  all_different([x[i,j] | i,j in 1..n]) :: domain
  /\ % the rook moves
  forall(k in 1..n*n-1) (
     next(x, k, k+1) 
  )
  /\ next(x, 1, n*n) % make it a tour
;

output[
 "["
]
++
[
  if j = 1 then ",\n" else "," endif ++
    if fix(x[i,j]) < 10 then " " else "" endif ++
    show(x[i,j])
  | i, j in 1..n
] ++ ["],\n"]
;