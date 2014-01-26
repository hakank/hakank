% 
% Magic square primes in MiniZinc.
% 
% Magic square with only prime numbers,
% with the additional objective of minimizing total
% (or minimizing the largest used value).
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5;

int: num_primes = 168;
set of int: primes = 
{
2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,
137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,
269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,
419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,
571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,
727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,863,877,881,
883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997
};

% decision variables
array[1..n,1..n] of var primes: x;
var 0..max(primes)*n*n: total;

% solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;
solve :: int_search(
        [x[i,j] | i in 1..n, j in 1..n], 
        first_fail, % first_fail,
        indomain_random, % indomain_median,
        complete) 
    % satisfy;
    minimize total;
    % minimize(max([x[i,j] | i,j in 1..n]));

constraint
  trace("ub(total): " ++ show(ub(total)) ++ "\n", true)
;

constraint
  all_different([x[i,j] | i in 1..n, j in 1..n]) :: domain % :: bounds % domain
  /\
  forall(k in 1..n) (
     sum(i in 1..n) (x[k,i]) = total /\
     sum(i in 1..n) (x[i,k]) = total
  )
  /\ % diagonal
  sum(i in 1..n) (x[i,i]) = total
  /\ % diagonal
   sum(i in 1..n) (x[i,n-i+1]) = total

;

output 
[
  "total: " ++ show(total)
]
++
[
  if j = 1 then "\n" else " " endif ++
    show_int(3, x[i,j])
  | i,j in 1..n
];

