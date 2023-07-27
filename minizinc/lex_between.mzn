% 
% Global constraint lex_between in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Clex_between.html
% """
% lex_between​(LOWER_BOUND,​VECTOR,​UPPER_BOUND)
%
% Purpose
%
% The vector VECTOR is lexicographically greater than or equal to the fixed 
% vector LOWER_BOUND and lexicographically smaller than or equal to the 
% fixed vector UPPER_BOUND.
%
% Example
%     (
%     <5, 2, 3, 9>,
%     <5, 2, 6, 2>,
%     <5, 2, 6, 3>
%     )
% 
% The lex_between constraint holds since:
%   * The vector VECTOR=<5, 2, 6, 2> is greater than or equal to the 
%     vector LOWER_BOUND=<5, 2, 3, 9>.
%   * The vector VECTOR=<5, 2, 6, 2> is less than or equal to the 
%      vector UPPER_BOUND=<5, 2, 6, 3>.
%
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 4;
array[1..n] of var 1..9: lower_bound;
array[1..n] of var 1..9: vector;
array[1..n] of var 1..9: upper_bound;

predicate lex_between(array[int] of var int: lower_bound,
                      array[int] of var int: vector,
                      array[int] of var int: upper_bound) =
  lex_less(lower_bound, vector)
  /\
  lex_less(vector, upper_bound)  
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint
  cp1d(lower_bound, [5, 2, 3, 9])
  /\
  cp1d(vector, [5, 2, 6, 2])
  /\
  cp1d(upper_bound, [5, 2, 6, 3])
  /\
  lex_between(lower_bound, vector, upper_bound)
;

output
[
  show(vector)
];
