% 
% Global constraint in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cdiffer_from_at_least_k_pos.html
% """
% Constraint
%
%   differ_from_at_least_k_pos​(K,​VECTOR1,​VECTOR2)​
% 
% ...
% Purpose
%
%     Enforce two vectors VECTOR1 and VECTOR2 to differ from at least 
%     K positions.
%
% Example
%     (
%     2, <2, 5, 2, 0>,
%        <3, 6, 2, 1>
%     )
%
%     The differ_from_at_least_k_pos constraint holds since the first 
%     and second vectors differ from 3 positions, which is greater 
%     than or equal to K=2.
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 4;
array[1..n] of var 0..6: a1;
array[1..n] of var 0..6: a2;
var 0..n: k;

predicate differ_from_at_least_k_pos(var int: k, 
                                     array[int] of var int: vector1,
                                     array[int] of var int: vector2) =
      sum(i in index_set(vector1)) (
         bool2int(a1[i] != a2[i])
      ) >= k
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 


solve satisfy;


constraint
   cp1d(a1,[2,5,2,0])
   /\
   cp1d(a2,[3,6,2,1])
   /\
   k = 2
   /\
   differ_from_at_least_k_pos(k, a1,a2)
;

output
[
  "a1: " ++ show(a1) ++ "\n" ++
  "a2: " ++ show(a2) ++ "\n" ++
  "k: " ++ show(k) ++ "\n"
];
