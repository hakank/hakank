% 
% Global constraint k_same_modulo in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Ck_same_modulo.html
% """
% k_same_modulo(SETS, M)
%
% Purpose
%
% Given a collection of |SETS| sets, each containing the same number of 
% domain variables, the k_same_modulo constraint enforces a same_modulo 
% constraint between each pair of consecutive sets.
%
% Example
%     (
%     <
%     set-<1, 9, 1, 5, 2, 1>,
%     set-<6, 4, 1, 1, 5, 5>,
%     set-<1, 3, 4, 2, 8, 7>
%     >, 3
%     )
%
% The k_same_modulo constraint holds since:
%  * The first and second collections of variables are assigned 1 value in 
%    {0, 3, ..., 3·k} , 3 values in  {1, 4, ..., 1+3·k}  and 2 values in  
%     {2, 5, ..., 2+3·k} .
%  * The second and third collections of variables are also assigned 1 value 
%    in {0, 3, ..., 3·k} , 3 values in  {1, 4, ..., 1+3·k}  and 2 values in  
%    {2, 5, ..., 2+3·k} .
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 6;
int: r = 3;
array[1..r, 1..n] of var 1..9: v;
int: M = 3;

%
% from same_modulo.mzn
%
predicate same_modulo_pair(array[int] of var int: v1, 
                      array[int] of var int: v2, 
                      int: M) =
  let {
    int: lbv1 = min(index_set(v1)),
    int: ubv1 = max(index_set(v1)),
    array[lbv1..ubv1] of var 0..M-1: m1,
    array[lbv1..ubv1] of var 0..M-1: m2,
    array[0..M-1] of var 0..ubv1: gcc1,
    array[0..M-1] of var 0..ubv1: gcc2
  }
  in
  forall(i in index_set(v1)) (
     m1[i] = v1[i] mod M
     /\
     m2[i] = v2[i] mod M
  ) 
  /\
  global_cardinality(m1, array1d(0..M-1, set2array(index_set(gcc1))), gcc1)
  /\
  global_cardinality(m2, array1d(0..M-1, set2array(index_set(gcc2))), gcc2)
  /\
  cp1d(gcc1, gcc2)

;


predicate same_modulo(array[int,int] of var int: v, int: M) =
  forall(i,j in index_set_1of2(v) where i < j) (
     same_modulo_pair([v[i, k] | k in index_set_2of2(v)], [v[j, k] | k in index_set_2of2(v)], M)
  )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
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

solve :: int_search([v[i,j] | i in 1..r, j in 1..n] , first_fail, indomain_min, complete) satisfy;

constraint
  cp2d(v, array2d(1..r, 1..n, 
       [
        1,9,1,5,2,1,
        % _,9,1,5,2,1, % test
        6,4,1,1,5,5,
        1,3,4,2,8,7
       ]))
  /\
  same_modulo(v, M)
;

output
[
   if j = 1 then "\n" else " " endif ++
     show(v[i,j])
   | i in 1..r, j in 1..n
];
