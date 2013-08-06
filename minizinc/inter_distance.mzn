% 
% Inter-distance constraint in MiniZinc.
% 
% From 
% Claude-Guy Quimper: "Efficient Propagators for Global Constraints"
% page 3:
% """
% The Inter-Distance constraint ensures that all variables, among a set 
% of variables x1,...,xn, are pairwise distant from p,
% i.e. |xi-xj| >= p for all i != j. 
% """
%
% Note: when p = 1 then inter_distance is the same as alldifferent.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 8;
array[1..n] of var 0..n*4: x;
var 1..10: p;

predicate inter_distance(array[int] of var int: x, var int: p) =
  forall(i,j in 1..n where i < j) (
       abs(x[i]-x[j]) >= p
  )
; 

solve maximize p;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  increasing(x)
  /\
  inter_distance(x, p)
  /\
  p >= 2 
;

output
[ 
  "p: " ++ show(p) ++ "\n" ++
  "x: " ++ show(x) ++ "\n"
]
++
["\n"];