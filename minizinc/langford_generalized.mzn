% Langford's number problem (generalized) in MiniZinc.
%
% Langford's number problem (CSP lib problem 24)
% http://www.csplib.org/Problems/prob024/
% """
% Consider two sets of the numbers from 1 to 4. The problem is to arrange the eight numbers 
% in the two sets into a single sequence in which the two 1’s appear one number apart, the two 
% 2’s appear two numbers apart, the two 3’s appear three numbers apart, and the two 4’s appear four numbers apart.
% 
% The problem generalizes to the L(k,n) problem, which is to arrange k sets of numbers 1 to n, 
% so that each appearance of the number m is m numbers on from the last. For example, the 
% L(3,9) problem is to arrange 3 sets of the numbers 1 to 9 so that the first two 1’s and 
% the second two 1’s appear one number apart, the first two 2’s and the second two 2’s appear 
% two numbers apart, etc.
% """
% 
% This version (compared to http://hakank.org/minizinc/langford2.mzn) generalizes
% the number of sets, m >= 2.
%
%  
% Model created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

%
% The unique solution (with symmetry breaking) for n = 4, k=2:
% [2, 3, 4, 2, 1, 3, 1, 4]
%
% The three solutions for n=9, k=3 (with symmetry breaking)
% [1, 9, 1, 2, 1, 8, 2, 4, 6, 2, 7, 9, 4, 5, 8, 6, 3, 4, 7, 5, 3, 9, 6, 8, 3, 5, 7]
% ----------
% [1, 8, 1, 9, 1, 5, 2, 6, 7, 2, 8, 5, 2, 9, 6, 4, 7, 5, 3, 8, 4, 6, 3, 9, 7, 4, 3]
% ----------
% [1, 9, 1, 6, 1, 8, 2, 5, 7, 2, 6, 9, 2, 5, 8, 4, 7, 6, 3, 5, 4, 9, 3, 8, 7, 4, 3]
% 
% One solution of (18,3):
% [5, 18, 12, 6, 17, 10, 5, 16, 14, 4, 6, 9, 5, 15, 4, 12, 10, 6, 13, 4, 18, 9, 17, 14, 16, 11, 7, 10, 12, 15, 2, 9, 13, 2, 7, 8, 2, 11, 14, 18, 17, 16, 7, 3, 8, 15, 13, 3, 1, 11, 1, 3, 1, 8]
%

include "globals.mzn";

int: n = 10; % the digits
int: k = 3; % number of occurrences of each number

% decision variables
array[1..k*n] of var 1..n: a;     % the sequence
array[1..k*n] of var 1..k*n: pos; % positions


% solve satisfy;
solve :: int_search(pos, first_fail, indomain_split, complete) satisfy;

% constraint
%   assert(n mod k == 0, "n (" ++ show(k) ++ ") must be a multiple of n (" ++ show(n) ++ ")")  
% ;

constraint
  all_different(pos) /\

  forall(i in 1..n) (
    let {
       var 1..k*n - ((k-1)*i): j;
    } in 
    % exists(j in 1..k*n - ((n-1)*i)) (
       forall(c in 0..k-1) (
         a[j+(i*c)+c] = i /\
         pos[(i-1)*k+c+1] = j+(i*c)+c
       )
    % )
  )

  % exactly m occurrences of each number
  /\ global_cardinality(a, [i | i in 1..n], [k  | i in 1..n])

  % Symmetry breaking
  /\ a[1] < a[k*n]
    
;

output [
  "a: ", show(a), "\n",
   % "pos: ", show(pos), "\n"
];
