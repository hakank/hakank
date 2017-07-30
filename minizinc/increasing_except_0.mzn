% 
% Global constraint increasing_except_0 (decomposition) in MiniZinc.
% 
% This (decomposition) ensures that array x is increasing 
% if we disregard any 0's
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5;
array[1..n] of var 0..n: x;


% 
% Ensures that array a is increasing if we disregard any 0's
% 
predicate increasing_except_0(array[int] of var int: a) =
    forall(i,j in index_set(a) where i < j) (
       (a[i] != 0 /\ a[j] != 0) -> (a[i] <= a[j])
    )
;

predicate strictly_increasing_except_0(array[int] of var int: a) =
    forall(i,j in index_set(a) where i < j) (
       (a[i] != 0 /\ a[j] != 0) -> (a[i] < a[j])
    )
;


% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  increasing_except_0(x)
  % strictly_increasing_except_0(x)
;

output ["x: ", show(x), "\n"];

