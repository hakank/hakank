% 
% Pairwise sum problem in MiniZinc.
% 
% From Stack Overflow
% Pairwise sum of n numbers in non increasing order
% http://stackoverflow.com/questions/8566534/pairwise-sum-of-n-numbers-in-non-increasing-order
% """
% I saw this question in a programming interview blog 
%    [http://shashank7s.blogspot.com/2011/12/if-pairwise-sums-of-n-numbers-are-given.html]
%
% If pairwise sums of n numbers are given in non-decreasing 
% order identify the individual numbers. If the sum is corrupted print -1.
%
% Example:
%
% i/p: 4 5 7 10 12 13 
%
% o/p: 1 3 4 9
%
% A hint would suffice.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 6;
% array[1..n] of var 1..13: input; %  = [4, 5, 7, 10, 12, 13 ];
array[1..n] of int: input = [4, 5, 7, 10, 12, 13 ];

int: m = 4;
array[1..m] of var 0..ub_array(input): x;

% solve satisfy;
solve :: int_search(x, anti_first_fail, indomain_split, complete) satisfy;

% arrays of 1d where both arguments are var int
predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


% For generating all variants
% constraint
%    trace("ub_array(input): " ++ show(ub_array(input)) ++ "\n", 1=1) /\
%    forall(i in 2..n) (
%      input[i-1] < input[i]
%    )
%    % /\ % cp1d(input,  [4, 5, 7, 10, 12, 13 ])
% ;

constraint
   forall(k in 1..n) (
     let {
      var 1..m: i,
      var 1..m: j
     } in
      i < j /\
      x[i]+x[j] = input[k]
   )
   /\ increasing(x) % symmetry breaking
   % /\ alldifferent(x)
;

% Alternative but slower:
% constraint
%    forall(k in 1..n) (
%      exists(i, j in 1..m where i < j) ( 
%         x[i]+x[j] = input[k]
%      )
%    )
%    /\ increasing(x) % symmetry breaking
%    % /\ alldifferent(x)
% ;


output [
  "input: " ++ show(input) ++ "\n" ++
  "x    : " ++ show(x) ++ "\n"  
]
 ++ ["\n"]
;

