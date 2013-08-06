% 
% Global constraint lex_chain_less in MiniZinc.
% 
% From Global Constraint Catalogue
% http://www.emn.fr/x-info/sdemasse/gccat/Clex_chain_less.html
% """
% lex_chain_less​(VECTORS)​
%
% Purpose
%
% For each pair of consecutive vectors VECTORi and VECTORi+1 of the 
% VECTORS collection we have that VECTORi is lexicographically strictly 
% less than VECTORi+1. Given two vectors, X and Y of n components, 
% 〈X0,​...,​Xn−1〉 and 〈Y0,​...,​Yn−1〉, X is lexicographically strictly less 
% than Y if and only if X0​<Y0 or X0=Y0 and 〈X1,​...,​Xn−1〉 is 
% lexicographically strictly less than 〈Y1,​...,​Yn−1〉.
%
% Example
%     (
%     <
%     vec-<5, 2, 3, 9>,
%     vec-<5, 2, 6, 2>,
%     vec-<5, 2, 6, 3>
%     >
%     )
%
%  The lex_chain_less constraint holds since:
%  * The first vector 〈5,​2,​3,​9〉 of the VECTORS collection is 
%    lexicographically strictly less than the second vector 
%    〈5,​2,​6,​2〉 of the VECTORS collection.
%  * The second vector 〈5,​2,​6,​2〉 of the VECTORS collection is 
%    lexicographically strictly less than the third vector 
%    〈5,​2,​6,​3〉 of the VECTORS collection.
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: r = 3;
int: c = 4;
array[1..r, 1..c] of var 1..9: x;

predicate lex_chain_less(array[int, int] of var int: x) =

 forall(i in 2..card(index_set_1of2(x))) (
     (lex_less( [x[i-1, j] | j in index_set_2of2(x)], [x[i, j] | j in index_set_2of2(x)]))
  )
;

predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 

solve satisfy;


constraint
  cp2d(x, array2d(1..r, 1..c, 
     [
       5,2,3,9,
       5,2,6,2,
       5,2,6,3
     ]))
  /\
  lex_chain_less(x)
;

output [
 if j = 1 then "\n" else " "  endif ++
   show(x[i,j])
 | i in 1..r, j in 1..c
] ++ ["\n"]
;