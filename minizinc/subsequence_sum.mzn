% 
% Subsequence sum in MiniZinc.
% 
% A sequence sub_seq is a subsequence of seq when all values in sub_seq are 
% exactly in the same order as in seq. 
%
% The minimum (maximum) subsequence sum problem is to find the minimal (maximal)
% sum and the corresponding subsequence given a specific length of a 
% subsequence.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 6; % length of (master) sequence
int: m = 4; % length of sub sequence
array[1..n] of var 1..n: x; % master sequence
array[1..m] of var 1..n: x_s; % subsequence
var int: ss_sum;

% solve satisfy;
% solve maximize ss_sum;
solve minimize ss_sum;


%
% sums the sequence sub_seq in an sub sequence of seq
%
predicate subsequence_sum(array[int] of var int: seq, array[int] of var int: sub_seq, var int: sub_seq_sum) =
   let {
      array[1..length(sub_seq)] of var 1..length(seq): pos
   }
   in
   % identify the positions in seq for all elements in sub_seq
   forall(i in 1..length(sub_seq)) (
     let {
       var i..length(seq): j 
     }
     in
       pos[i] = j
       /\
       seq[j] = sub_seq[i]
   )
   /\ % ensure that all positions are in increasing order
   forall(i in 2..length(sub_seq)) (
     pos[i-1] < pos[i]
   )
   /\ % and sums the subsequence
   sub_seq_sum = sum(sub_seq)
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

constraint

   cp1d(x,[5,4,1,5,2,1])
   /\ 
   subsequence_sum(x,x_s, ss_sum)
;

output [
  "x: ", show(x), "\n",
  "x_s: ", show(x_s), "\n",
  "ss_sum: ", show(ss_sum), "\n",
];