% 
% Global constraint nclass in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cnclass.html
% """
% nclass​(NCLASS,​VARIABLES,​PARTITIONS)
% 
% Purpose
%
% Number of partitions of the collection PARTITIONS such that at least one 
% value is assigned to at least one variable of the collection VARIABLES.
% 
% Example
%  (
%   2, <3, 2, 7, 2, 6>,
%   <
%     p-<1, 3>,
%     p-<4>,
%     p-<2, 6>
%   >
%   )
%
% Observe that the values of <3, 2, 7, 2, 6> occur within partitions 
% p-<1, 3> and p-<2, 6> but not within p-<4>. Consequently, the nclass 
% constraint holds since its first argument NCLASS is set to value 2.
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 5;
array[1..n] of var 1..7: x;
var 0..n: num_class;
array[1..3] of var set of 1..7: s;

predicate nclass(var int: nc, 
                 array[int] of var int: v, 
                 array[int] of var set of int: s) =
   all_disjoint(s)
   /\
   num_class = sum(i in index_set(s)) (
     bool2int(
        exists(j in index_set(v)) (
           v[j] in s[i]
        )
     )
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

predicate cp1d(array[int] of var set of int: x, array[int] of var set of int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
   cp1d(s, [
          {1,3},
          {4},
          {2,6}
       ])
   /\
   cp1d(x, [3,2,7,2,6])
   /\
   num_class = 2
   /\
   nclass(num_class, x, s)
;

output
[
  "x: " ++ show(x) ++ "\n" ++
  "s: " ++ show(s) ++ "\n" ++
  "num_class: " ++ show(num_class)

];
