% 
% All different pairs in MiniZinc.
% 
% Assumption: a is a k by 2 matrix. n is the number of nodes.
%
% This model implements these decompositions:
%
%  - pairs(x,n): function which returns the pairs of matrix x
%    in "integer representation": a[k,1]*(n-1) + a[k,2]
%
%  - all_different_pairs(x,n): all the pairs in x must be different
%
%  - increasing_pairs(x,n): the pairs in x is in increasing order

%  - decreasing_pairs(x,n): the pairs in x is in decreasing order

% n  #solutions
% -------------
% 1      0
% 2      1
% 3     12 
% 4    377
% 5  53834
% 6

% 0,1,12,377,53834

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 4;
int: m = n*(n-1) div 2;

% decision variables
array[1..m, 1..2] of var 1..n: x;

% all pairs must be different
predicate all_different_pairs(array[int,int] of var int: a, int: s) =
  all_different(pairs(a,s))
;

% the pairs are in increasing order
predicate increasing_pairs(array[int,int] of var int: a, int: s) = 
  increasing(pairs(a,s)) 
;

% the pairs are in decreasing order
predicate decreasing_pairs(array[int,int] of var int: a, int: s) = 
  decreasing(pairs(a,s)) 
;

% return the pairs of a in the "integer representation": a[k,1]*(n-1) + a[k,2]
function array[int] of var int: pairs(array[int,int] of var int: a, int: s) =
  [ a[k,1]*(s-1) + a[k,2] | k in index_set_1of2(a)]
;

% solve satisfy;
solve :: int_search(array1d(x), first_fail, indomain_min, complete) satisfy;

constraint
  all_different_pairs(x,n) 
  /\ increasing_pairs(x,n) 
  % /\ forall(k in 1..m) ( x[k,1] < x[k,2]) 
  /\ forall(k in 1..m) ( x[k,1] != x[k,2]) 
;

output 
[
  show([x[k,j] | j in 1..2]) ++ "\n"
  | k in 1..m
]
++
[
 "pairs: \([ (x[k,1]*(n-1)) + x[k,2] | k in index_set_1of2(x)])\n",
]
;
