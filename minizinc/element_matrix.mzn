% 
% Global constraint in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Celement_matrix.html
% """
% Constraint
%
%     element_matrix(MAX_I,MAX_J,INDEX_I,INDEX_J,MATRIX,VALUE)
%
% Purpose
%
%     The MATRIX collection corresponds to the two-dimensional matrix 
%     MATRIX[1..MAX_I,1..MAX_J]. VALUE is equal to the entry MATRIX[INDEX_I,INDEX_J] of the previous matrix.

% Example
%     (
%     4,3,1,3,<
%     i-1 j-1 v-4,
%     i-1 j-2 v-1,
%     i-1 j-3 v-7,
%     i-2 j-1 v-1,
%     i-2 j-2 v-0,
%     i-2 j-3 v-8,
%     i-3 j-1 v-3,
%     i-3 j-2 v-2,
%     i-3 j-3 v-1,
%     i-4 j-1 v-0,
%     i-4 j-2 v-0,
%     i-4 j-3 v-6
%     >,7
%     )
%
%     The element_matrix constraint holds since its last argument 
%     VALUE=7 is equal to the v attribute of the kth item of the MATRIX 
%     collection such that MATRIX[k].i=INDEX_I=1 and MATRIX[k].j=INDEX_J=3.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: max_i = 4;
int: max_j = 3;
var 1..max_i: index_i;
var 1..max_j: index_j;
array[1..max_i,1..max_j] of var 0..9: matrix;
var 0..8: value;

predicate element_matrix(var int: index_i, 
                         var int: index_j, 
                         array[int,int] of var int: matrix,
                         var int: value) =
                   matrix[index_i,index_j] = value   
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
  
   cp2d(matrix,array2d(1..max_i,1..max_j, [
                        4,1,7,
                        1,0,8,
                        3,2,1,
                        0,0,6
                    ]))
   /\
   index_i = 1
   /\ 
   index_j = 3
   /\
    value = 7
   /\
   element_matrix(index_i,index_j,matrix, value)
;


output
[
  "index_i: " ++ show(index_i) ++ "\n" ++
  "index_j: " ++ show(index_j) ++ "\n" ++
  "value: " ++ show(value) ++ "\n" 
];
