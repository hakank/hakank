% 
% Global constraint lex_different in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Clex_different.html
% """
% lex_different​(VECTOR1,​VECTOR2)
%
% Purpose
%
% Vectors VECTOR1 and VECTOR2 differ in at least one component.
%
% Example
%     (
%     <5, 2, 7, 1>,
%     <5, 3, 7, 1>
%     )
%
% The lex_different constraint holds since VECTOR1 = <5, 2, 7, 1> 
% and VECTOR2 = <5, 3, 7, 1> differ in their second component.
%
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 4;
array[1..n] of var 1..7: vector1;
array[1..n] of var 1..7: vector2;

predicate lex_different(array[int] of var int: vector1,
                        array[int] of var int: vector2) =
  sum(i in index_set(vector1)) (bool2int(vector1[i] != vector2[i])) > 0
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
  cp1d(vector2, [5,3,7,1])
  /\
  lex_different(vector1, vector2)

;


output
[
  "vector1: " ++ show(vector1) ++ "\n" ++
  "vector2: " ++ show(vector2) ++ "\n" 
];
