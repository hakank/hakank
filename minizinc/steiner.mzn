% 
% Steiner triplets in MiniZinc.
% 
% http://www.probp.com/examples/clpset/steiner.pl 
% The ternary Steiner problem of order n is to find n(n-1)/6 sets of elements in {1,2,...,n} 
% such that each set contains three elements and any two sets have at most one element in common. 
% For example, the following shows a solution for size n=7:
%
%      {1,2,3}, {1,4,5}, {1,6,7}, {2,4,6}, {2,5,7}, {3,4,7}, {3,5,6}
%
% Problem taken from:
%  C. Gervet: Interval Propagation to Reason about Sets: Definition and Implementation of a Practical 
%  Language,  Constraints, An International Journal, vol.1, pp.191-246, 1997.
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 


include "globals.mzn";

int: N = 7; 
int: NB = N *(N-1) div 6;
array[1..NB] of var set of 1..N: Sets;

% solve satisfy;
solve::set_search(Sets, first_fail, indomain_min, complete) satisfy;



constraint
   forall(i in index_set(Sets)) (
      card(Sets[i]) = 3
   )
   /\ 
   forall(i,j in index_set(Sets) where i < j) (
      card( Sets[i] intersect Sets[j]) <= 1
   )
   /\ % symmetry breaking
   decreasing(Sets)

;

output [
       "N: ", show(N), " NB: ", show(NB), "\n",
       "Sets: ", show(Sets)
];

