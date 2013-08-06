% 
% Subsequence in MiniZinc.
% 
% A sequence sub_seq is a subsequence of seq when all values in sub_seq are 
% exactly in the same order as in seq.
% 
% 
% MiniZinc model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 6;
int: m = 4;
array[1..n] of var 1..n: x;
array[1..m] of var 1..n: x_s;

% array[1..n] of var 1..n: y;
% array[1..m] of var 1..n: y_s;

solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;


%
% sequence b in an subsequence of a
%
predicate subsequence(array[int] of var int: seq, array[int] of var int: sub_seq) =
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
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


constraint

   cp1d(x,[5,4,1,5,2,1])
   /\ 
   subsequence(x,x_s)
   /\
   cp1d(x_s, [5,4,2,1])
   % /\ subsequence(y,y_s)
   % /\
   % forall(i in 1..m) (x_s[i] = y_s[i])

;

output [
  "x: ", show(x), "\n",
  "x_s: ", show(x_s), "\n",
%  "y: ", show(y), "\n",
%  "y_s: ", show(y_s), "\n"
];