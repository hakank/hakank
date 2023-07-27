% 
% Global constraint k_same in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Ck_same.html
% """
% k_same​(SETS)
% 
% Purpose
%
% Given |SETS| sets, each containing the same number of domain variables, 
% the k_same constraint enforces that the multisets of values assigned to each set are all identical.
%
% Example
%     (
%     <
%     set-<1, 9, 1, 5, 2, 1>,
%     set-<9, 1, 1, 1, 2, 5>,
%     set-<5, 2, 1, 1, 9, 1>
%     >
%     )
%
% The k_same constraint holds since:
%
%  * The first and second collections of variables are assigned to the 
%     same multiset.
%
%  * The second and third collections of variables are also assigned to 
%    the same multiset.
% """


% Note: MiniZinc don't support multisets, so we use arrays instead.

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn";

int: n = 3;
int: m = 6;
array[1..n, 1..m] of var 1..9: x;

predicate k_same(array[int, int] of var int: x) =
   let {
      int: lbx1 = min(index_set_1of2(x)),
      int: ubx1 = max(index_set_1of2(x)),
      int: lbx2 = min(index_set_2of2(x)),
      int: ubx2 = max(index_set_2of2(x))

   } in
   forall(i in lbx1+1..ubx1) (
      let {
         int: low = lb_array(x),
         int: up  = ub_array(x),
         array[low..up] of var 0..up: i_gcc,
         array[low..up] of var 0..up: j_gcc
      }
      in
      global_cardinality([x[i,k] | k in lbx2..ubx2], set2array(index_set(i_gcc)), i_gcc) 
      /\
      global_cardinality([x[i-1,k] | k in lbx2..ubx2], set2array(index_set(j_gcc)), j_gcc)
      /\
      forall(k in low..up) (
         i_gcc[k] = j_gcc[k]
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

solve :: int_search([x[i,j] | i in 1..n, j in 1..m], anti_first_fail, indomain_min, complete) satisfy;

constraint
     
   cp2d(x, array2d(1..n, 1..m, [
      1,9,1,5,2,1,
      %_,9,_,_,_,1, % test
      9,1,1,1,2,5,
      5,2,1,1,9,1
  ]))

  /\ k_same(x)
;

output [
    if j = 1 then "\n" else " " endif ++
      show(x[i,j])
   | i in 1..n, j in 1..m
] ++ ["\n"];
