% 
% Global constraint atmost1 in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Catmost1.html
% """
% Given a collection of set variables s1, s2, ..., sn and their respective 
% cardinality c1, c2, ..., cn, the atmost1 constraint enforces the following 
% two conditions:
% [This is the MiniZinc version of the conditions:
%  forall(i in 1..n) ( 
%     card(s[i]) = x[i] 
%  )
%  /\
%  forall(i, j in 1..n where i < j) (
%    card(s[i] intersect s[j]) <= 1
%  )
% ]
% 
% Example
% (
%  <
%    s-{5, 8}   c-2,
%    s-{5}      c-1,
%    s-{5,6,7}  c-3,
%    s-{1,4}    c-2
%    >
% )
% 
% The atmost1 constraint holds since:
%  * |{5, 8}|=2, |{5}|=1, |{5, 6, 7}|=3, |{1, 4}|= 2.
%  * |{5, 8} intersect {5}| <= 1, |{5, 8} intersect {5, 6, 7}|<=1, |{5, 8} intersect {1,4}|<=1,
%    |{5} intersect {5, 6, 7}|<=1, |{5} intersect {1,4}|<=1,
%    |{5, 6, 7} intersect {1, 4}| <= 1.
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 4;
array[1..n] of var 1..4: x;
array[1..n] of var set of 1..8: s;

predicate atmost1(array[int] of var int: x, array[int] of var set of int: s) =

  forall(i in index_set(x)) ( 
     card(s[i]) = x[i] 
  )
  /\
  forall(i, j in index_set(x) where i < j) (
    card(s[i] intersect s[j]) <= 1
  )

;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
; 

predicate cp1d(array[int] of set of int: x, array[int] of var set of int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
      x[i] = y[i]
    )
  )
; 


solve satisfy;

constraint
  cp1d([2,1,3,2], x)
  /\
  cp1d([{5,8}, {5}, {5,6,7}, {1,4}],s)
  /\
  atmost1(x, s)
  % /\ % some symmetry breaking
  % lex_less([1,1,3,2],x)
;

output [
  "x: ", show(x), "\n",
  "s: ", show(s), "\n",
] ++ ["\n"];
