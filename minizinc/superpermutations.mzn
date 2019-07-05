% 
% Superpermutations in MiniZinc.
% 
%   https://en.wikipedia.org/wiki/Superpermutation
% """
% In combinatorial mathematics, a superpermutation on n symbols is a string that contains each permutation 
% of n symbols as a substring. While trivial superpermutations can simply be made up of every permutation 
% listed together, superpermutations can also be shorter (except for the trivial case of n = 1) because overlap 
% is allowed. For instance, in the case of n = 2, the superpermutation 1221 contains all possible permutations 
% (12 and 21), but the shorter string 121 also contains both permutations.
%
% It has been shown that for 1 ≤ n ≤ 5, the smallest superpermutation on n symbols has length 1! + 2! + … + n! 
% (sequence A180632 in the OEIS). The first four smallest superpermutations have respective 
% lengths 1, 3, 9, and 33, forming the strings 1, 121, 123121321, and 123412314231243121342132413214321. 
% However, for n = 5, there are several smallest superpermutations having the length 153. One such 
% superpermutation is shown below, while another of the same length can be obtained by switching all of 
% the fours and fives in the second half of the string (after the bold 2):[1]
%
% 12345123415234125341235412314523142531423514231542312453124351243152431254312 
% 1345213425134215342135421324513241532413524132541321453214352143251432154321 
% """
%
% See also:
%
% - http://www.gregegan.net/SCIENCE/Superpermutations/Superpermutations.html
%
% Gecode:
%
%  n=3: 0.072s
%  x: [1, 2, 3, 1, 2, 1, 3, 2, 1]
%  ps: [1, 5, 2, 6, 3, 7]
% 
%  n=4: 
%    input_order/indomain_split: 1.52s     12 threads: 0.793s
%    
%  x: [1, 2, 3, 4, 1, 2, 3, 1, 4, 2, 3, 1, 2, 4, 3, 1, 2, 1, 3, 4, 2, 1, 3, 2, 4, 1, 3, 2, 1, 4, 3, 2, 1]
%  ps: [1, 17, 6, 2, 22, 11, 27, 23, 18, 7, 3, 19, 12, 28, 24, 13, 8, 4, 20, 9, 29, 25, 14, 30]
% 
% 
%  n=5:
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n;
int: num_perms = factorial(n);
array[1..num_perms,1..n] of int: permutations;
int: upper_bound = sum([factorial(i) | i in 1..n]);

function int: factorial(int: n) =
  let {
    int: f = product([i | i in 1..n]);
  } in
  f
;

% decision variables
array[1..upper_bound] of var 1..n: x;
array[1..num_perms] of var 1..upper_bound: ps; % where start the p'th permutation
var 0..upper_bound: z = max(ps);

% solve minimize z;
% solve :: int_search(x ++ ps, input_order, indomain_split, complete) minimize z;
solve :: int_search(x ++ ps, occurrence, indomain_split, complete) minimize z;

constraint
  forall(perm in 1..num_perms) (
    forall(j in 0..n-1) (
      x[ps[perm]+j] = permutations[perm,j+1]
    )  
  )

  /\ all_different(ps)


  /\ % symmetry breaking
  forall(i in 1..n) (
    x[i] = i
  )


  /\ % palindrome (experimental)
  forall(i in 1..upper_bound) (
    x[i] = x[upper_bound-i+1]
  )
  % forall(i in 1..z+n-1) (
  %   x[i] = x[z+n-1-i+1]
  % )
;

output [
  "factorial(n): \(factorial(n))\n",
  "upper_bound: \(upper_bound)\n",
  "x: \(x)\n",
  "ps: \(ps)\n",
  "z: \(z)\n",
  "tot_len: \(z+n-1)\n",
];
