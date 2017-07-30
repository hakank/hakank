% 
% Maximum subarray problem in MiniZinc.
% 
% http://www.rosettacode.org/wiki/Maximum_subarray
% """
% Given an array of integers, find a subarray which maximizes the sum of its elements, 
% that is, the elements of no other single subarray add up to a value larger than this one. 
% An empty subarray is considered to have the sum 0; thus if all elements are negative, 
% the result must be the empty sequence.
% ...
% a1 = [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1];
% Answer_ 
%  Maximal subsequence: [3,5,6,-2,-1,4]
% (i.e. 3..8)
%
% auto a2 = [-1, -2, -3, -5, -6, -2, -1, -4, -4, -2, -1];
% answer: Maximal subsequence: []
% 
% """
%
 
%
% Cf subsequence_sum.mzn, which uses a fixed length of the subsequence.
% Cf minsum.mzn which use a simpler approach for a similar problem.
 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 11;
array[1..n] of var -10..10: a; % the sequence
array[1..n] of var -999..19: ans; % the sequence

var -10..100: tot_sum;

% index of the sequence
var 1..n: from;
var 1..n: to;

% Since an empty sequence should be shown if tot_sum < 0, we use
% these variable to contain the answer.
var 0..n: from_answer;
var 0..n: to_answer;

predicate cp1d(array[int] of var int: x, array[int] of int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve :: int_search(
        a ++ ans ++ [tot_sum, from, to, from_answer, to_answer], 
        first_fail, 
        indomain_min, 
        complete)
   maximize tot_sum;
% solve satisfy;

constraint
 %% two maximum sequence (tot_sum=6) 
 % cp1d(a, [1, 2, 3, -100, 1, 5] )
 % /\ tot_sum = 6

 cp1d(a, [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1])

 /\ % get the indices and sum the subsequence
 exists(i, j in 1..n where i < j) (
   from = i
   /\
   to = j
   /\
   tot_sum = sum(k in i..j) (
     a[k]
    )
  )
  /\
  ( % the answer 
    (tot_sum > 0 <-> from_answer = from /\ to_answer = to) 
    /\
    (tot_sum <= 0 <-> from_answer = 0 /\ to_answer = 0) 
 
  )
  /\ % the answer sequence
  forall(k in 1..n) (
       (k >= from_answer /\ k <= to_answer <-> ans[k] = a[k] )
       /\
       (k < from_answer \/ k > to_answer <-> ans[k] = -999)
  ) 
;


output [
  "a: ", show(a),"\n",
  "from: ", show(from), "\n",
  "to: ", show(to), "\n",
  "tot_sum: ", show(tot_sum), "\n",
  "from_answer: ", show(from_answer), "\n",
  "to_answer: ", show(to_answer), "\n",
  "ans: ", show(ans), "\n",
] ++
[
 if k >= fix(from_answer) /\ k <= fix(to_answer) then show(a[k]) else show(-999) endif ++ " "
 %  if k >= fix(from_answer) /\ k <= fix(to_answer) then show(a[k]) else "" endif ++ " "

 | k in 1..n
]
++ ["\n"]

;