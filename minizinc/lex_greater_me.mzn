% 
% Global constraint in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Clex_greater.html
% """
% lex_greater​(VECTOR1,​VECTOR2)
% 
% Purpose
%
% VECTOR1 is lexicographically strictly greater than VECTOR2. Given two 
% vectors, X and Y of n components, 〈X0,​...,​Xn−1〉 and 〈Y0,​...,​Yn−1〉, 
% X is lexicographically strictly greater than Y if and only if X0>​Y0 
% or X0=Y0 and 〈X1,​...,​Xn−1〉 is lexicographically strictly greater 
% than 〈Y1,​...,​Yn−1〉.
% 
% Example
%   (
%   <5, 2, 7, 1>,
%   <5, 2, 6, 2>
%   )
% 
% The lex_greater constraint holds since VECTOR1=<5, 2, 7, 1> is 
% lexicographically strictly greater than VECTOR2=<5, 2, 6, 2>.% 
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 4;
array[1..n] of var 1..7: vector1;
array[1..n] of var 1..7: vector2;

%
% This is just a convenient predicate
%
predicate lex_greater_me(array[int] of var int: x,
                     array[int] of var int: y) =
    lex_less(y, x)
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint
  cp1d(vector1, [5,2,7,1])
  /\
  cp1d(vector2, [5,2,6,2])
  /\
  lex_greater_me(vector1, vector2)

;


output [
 "vector1: ", show(vector1), "\n",
 "vector2: ", show(vector2), "\n",

];
