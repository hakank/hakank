% 
% Global constraint npair in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cnpair.html
% """
%     npair​(NPAIRS,​PAIRS)​
%
% Purpose
%
%     NPAIRS is the number of distinct pairs of values assigned 
%     to the pairs of variables of the collection PAIRS.

% Example
%     (
%     2,​<
%     x-3	y-1,
%     x-1	y-5,
%     x-3	y-1,
%     x-3	y-1,
%     x-1	y-5
%     >
%     )
%
%     The npair constraint holds since its first argument NPAIRS=2 is 
%     set to the number of distinct pairs <x-3 y-1> and <x-1 y-5> of 
%     its second argument PAIRS.
%
% Remark
%
%     This is an example of a number of distinct values constraint 
%     where there is more than one attribute that is associated with 
%     each vertex of the final graph.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 5;
array[1..n, 1..2] of var 1..5: pairs;
var lb_array(pairs)..ub_array(pairs): npairs;

predicate npair(var int: npairs, array[int,int] of var int: pairs) =
    let {
       array[lb_array(pairs)..ub_array(pairs)] of var 0..1 : first_pos
    } in
    forall(i in index_set_1of2(pairs)) (
       first_pos[i] = 1 <-> 
           forall(j in lb_array(pairs)..i-1) (
               not(
                   pairs[i,1] = pairs[j,1]
                   /\
                   pairs[i,2] = pairs[j,2]
               )
           )
    )
    /\
    npairs = sum(first_pos)
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
   npairs = 2 /\
   cp2d(pairs, array2d(1..n,1..2, [
                       3,1,
                       1,5,
                       3,1,
                       3,1,
                       1,5
                  ]))
    /\
    npair(npairs, pairs)
;

output
[
  "npairs: " ++ show(npairs) ++ "\n" ++ 
  "pairs: " ++ show(pairs) ++ "\n" 
];
