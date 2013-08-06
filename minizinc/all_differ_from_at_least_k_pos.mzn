% 
% Global constraint all_differ_from_at_least_k_pos in MiniZinc.
% 
% Global Constraint Catalogue
% http://www.emn.fr/z-info/sdemasse/gccat/Call_differ_from_at_least_k_pos.html
% """
% Enforce all pairs of distinct vectors of the VECTORS collection to differ 
% from at least K positions.
% 
% Example
% (
%  2, <
%  vec-<2, 5, 2, 0>,
%  vec-<3, 6, 2, 1>,
%  vec-<3, 6, 1, 0>
%  >
%)
% 
% The all_differ_from_at_least_k_pos constraint holds since:
%  * The first and second vectors differ from 3 positions, which is 
%    greater than or equal to K=2.
%  * The first and third vectors differ from 3 positions, which is greater 
%    than or equal to K=2.
%  * The second and third vectors differ from 2 positions, which is greater 
%    than or equal to K=2.
% """

% 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: rows;
int: cols;
array[1..rows, 1..cols] of var 0..6: x;
var 0..cols: c;

%
% all_differ_from_at_least_k_pos(k, x)
% 
predicate all_differ_from_at_least_k_pos(var int: k, array[int, int] of var int: vectors) =
  forall(i, j in index_set_1of2(vectors) where i < j) (
     sum(kk in index_set_2of2(vectors)) ( bool2int( vectors[i,kk] != vectors[j,kk] )) >= k
  )
;

% copy 
predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
        y[i,j] = x[i,j]
    ) 
  )
; 

% solve satisfy;
solve :: int_search(
        [x[i,j] | i in 1..rows, j in 1..cols],
        first_fail,
        indomain_min,
        complete)
    satisfy;


constraint
  cp2d(array2d(1..rows, 1..cols, 
                  [2,5,2,0,
                   3,6,2,1,
                   3,6,1,0]), x)
  /\

  % cp2d(array2d(1..rows, 1..cols, 
  %                  [0,0,0,0,
  %                   0,0,0,0,
  %                   0,0,0,0]), x)
  %  /\

  
  all_differ_from_at_least_k_pos(c, x)
  /\
  c = 2 % with fixed "k"
;


output 
[ "\nc: ", show(c)
] ++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i in 1..rows, j in 1..cols
] ++ ["\n"];



%
% data
%
rows = 3;
cols = 4;