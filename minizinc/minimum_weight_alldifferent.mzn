% 
% Global constraint minimum_weight_alldifferent in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cminimum_weight_alldifferent.html
% """
% Constraint
%
%     minimum_weight_alldifferent (VARIABLES, MATRIX, COST) 
%
% Purpose
%
%     All variables of the VARIABLES collection should take a distinct 
%     value located within interval [1,|VARIABLES|]. In addition COST 
%     is equal to the sum of the costs associated with the fact that 
%     we assign value i to variable j. These costs are given by 
%     the matrix MATRIX.
%
% Example
%     (
%     <2, 3, 1, 4>,
%     <
%     i-1 j-1 c-4,
%     i-1 j-2 c-1,
%     i-1 j-3 c-7,
%     i-1 j-4 c-0,
%     i-2 j-1 c-1,
%     i-2 j-2 c-0,
%     i-2 j-3 c-8,
%     i-2 j-4 c-2,
%     i-3 j-1 c-3,
%     i-3 j-2 c-2,
%     i-3 j-3 c-1,
%     i-3 j-4 c-6,
%     i-4 j-1 c-0,
%     i-4 j-2 c-0,
%     i-4 j-3 c-6,
%     i-4 j-4 c-5
%     >, 17
%     )
%
% The minimum_weight_alldifferent constraint holds since the cost 
% 17 corresponds to the sum 
%   MATRIX[(1-1)*4+2].c+ MATRIX[(2-1)*4+3].c+ MATRIX[(3-1)*4+1].c+ MATRIX[(4-1)*4+4].c=
%   MATRIX[2].c+ MATRIX[7].c+ MATRIX[9].c+ MATRIX[16].c=
%  1+8+3+5.
% 
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 4;
array[1..n] of var 1..n: variables;
array[1..n, 1..n] of var 0..9: matrix;
var 0..1000: cost;

predicate minimum_weight_alldifferent(array[int] of var int: variables,
                                      array[int,int] of var int: matrix,
                                      var int: cost) =
   all_different(variables)
   /\
   cost = sum(i in index_set(variables)) (
      sum(j in index_set_2of2(matrix)) (
         bool2int(variables[i] = j)*matrix[i,j]
      )
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

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve minimize cost;

constraint
   cp1d(variables, [2,3,1,4]) % /\
   % cp2d(matrix, array2d(1..n, 1..n, [
   %                 4,1,7,0,
   %                 1,0,8,2,
   %                 3,2,1,6,
   %                 0,0,6,5
   %             ]))
   /\
   cost = 17
   /\
   minimum_weight_alldifferent(variables, matrix, cost)
;


output
[
  "variables: " ++ show(variables) ++ "\n" ++
  "cost: " ++ show(cost) ++ "\n" ++
  "matrix: " ++ show(matrix) ++ "\n"
];
