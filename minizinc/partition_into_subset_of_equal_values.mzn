% 
% Partition into subset of equal sums in MiniZinc.
% 
% From Programmers Stack Exchange (C#)
% http://programmers.stackexchange.com/questions/153184/partitioning-set-into-subsets-with-respect-to-equality-of-sum-among-subsets
% Partitioning set into subsets with respect to equality of sum among subsets
% """
% let say i have {3, 1, 1, 2, 2, 1,5,2,7} set of numbers, I need to split the 
% numbers such that sum of subset1 should be equal to sum of subset2 
% {3,2,7} {1,1,2,1,5,2}. First we should identify whether we can split number(one 
% way might be dividable by 2 without any remainder) and if we can, we should 
% write our algorithm two create s1 and s2 out of s.
% 
% How to proceed with this approach? I read partition problem in wiki and even in some 
% articles but i am not able to get anything. Can someone help me to find the 
% right algorithm and its explanation in simple English?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 9;
int: num_subsets = 2;
array[1..n] of int: s = [3, 1, 1, 2, 2, 1, 5, 2, 7];

% decision variables

% to which subset does x[i] belong
array[1..n] of var 1..num_subsets: x;


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

% Hardcoded for 2 subsets
% constraint
%    sum([s[i]*bool2int(x[i] == 1) | i in 1..n]) == 
%    sum([s[i]*bool2int(x[i] == 2) | i in 1..n])
%    /\ % symmetry breaking
%    x[1] = 1
% ;

% More general
constraint
   forall(p in 1..num_subsets-1) (
      sum([s[i]*bool2int(x[i] == p) | i in 1..n]) == 
      sum([s[i]*bool2int(x[i] == p+1) | i in 1..n])
   )
;

% symmetry breaking
constraint
   x[1] = 1
;


output [
  "s: " ++ show(s) ++ "\n" ++
  "x: " ++ show(x) ++ "\n"
]
++
[
  "subset" ++ show(j) ++ ": " ++ show([s[i] | i in 1..n where fix(x[i]) == j]) ++ "\n"
  | j in 1..num_subsets
] 
;

