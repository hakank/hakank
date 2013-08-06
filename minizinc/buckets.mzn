% 
% Weight distribution problem in MiniZinc.
% 
% From http://jsoftware.com/pipermail/programming/2011-April/022538.html
% """
% When I looked at their web page, I came across a problem that one of the 
% Thalesians (Paul) had posted on their site. The problem asked for a 
% general solution to a problem where there is a number of masses of 
% different values which are to be distributed among a few buckets. The 
% problem is to distribute the masses such that the difference in mass 
% between the filled buckets is minimized. You can see the full problem at 
% the bottom of the Thalesian site home page. I won't copy the full 
% problem here from their web page, as it uses embedded GIF images for the 
% equations.

% To make the problem more concrete, Paul gives a specific example of the 
% problem. There are four buckets, and 20 masses. The masses are 23, 43, 
% 12, 54, 7, 3, 5, 10, 54, 55, 26, 9, 9, 43, 54, 1, 8, 6, 38, 33 
% respectively.  What distribution of the 20 masses gives the smallest 
% mass difference between the four buckets?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: num_buckets = 4;
int: num_masses = 20;
% let's guess that the optimal is around sum(masses) div 4
% and add some extra of that value...
% int: c = 10 + sum(masses) div num_buckets;
int: c = sum(masses);

array[1..num_masses] of int: masses =
    [23, 43,  12, 54, 7, 3, 5, 10, 54, 55, 26, 9, 9, 43, 54, 1, 8, 6, 38, 33];

array[1..num_masses] of var 1..num_buckets: x;

array[1..num_buckets] of var 0..c*num_buckets: bins;

array[1..num_buckets, 1..num_buckets] of var 0..c: diffs;

var 0..c: total_diffs = sum([diffs[i,j] | i, j in 1..num_buckets]);

% solve minimize total_diffs;
% solve satisfy;
solve :: int_search(
        bins ++ [diffs[i,j] | i,j in 1..num_buckets] ++ x, 
        largest, 
        indomain_min, % indomain_min, 
        complete) 
    minimize total_diffs;


constraint bin_packing(c, x, masses);

constraint
   % for control and it speeds up, esp. LazyFD, fzn2smt, and Chuffed
   sum(bins) = sum(masses)
;

constraint
   % collect the different masses in a bin
   forall(b in 1..num_buckets) (
      bins[b] = sum([masses[j]*bool2int(b=x[j]) | j in 1..num_masses])
   )
;

constraint
   forall(i, j in 1..num_buckets) (
      diffs[i,j] = abs(bins[i] - bins[j])
   )
;

% symmetry breaking
constraint
   decreasing(bins)
;

output [
  "x: " ++ show(x) ++ "\n" ++
  "bins: " ++ show(bins) ++ "\n" ++
  "total_diffs: " ++ show(total_diffs) ++ "\n"
] ++
[
  if j = 1 then "\n" else " " endif ++
    show(diffs[i,j])
  | i, j in 1..num_buckets
]
++ ["\n"] ++
[
  if j = 1 then "\n" ++ show(i) ++ ":" else "" endif ++  
  if fix(x[j]) = i then 
    show(j) ++ " (" ++ show(masses[j]) ++ ") "
  else 
    ""
  endif 
  | i in 1..num_buckets, j in 1..num_masses
] 
++ ["\n"]
;

