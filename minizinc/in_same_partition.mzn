% 
% Global constraint in_same_partition in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cin_same_parition.html
% """
% in_same_partition​(VAR1,​VAR2,​PARTITIONS)
% 
% Purpose
%
% Enforce VAR1 and VAR2 to be respectively assigned to values v1 and v2 that both belong 
% to a same partition of the collection PARTITIONS.
% 
% Example
%    (
%     6, 2, <
%     p-<1, 3>,
%     p-<4>,
%     p-<2, 6>
%     >
%     )
%
% The in_same_partition constraint holds since its first and second arguments 
% VAR1=6 and VAR2=2 both belong to the third partition <2, 6> of its third argument 
% PARTITIONS.
%
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 6; % range
int: m = 3; % num partitions

array[1..m] of var set of 1..n: s;
var 1..n: var1;
var 1..n: var2;

predicate in_same_partition(var int: v1, var int: v2, array[int] of var set of int: partitions) =
  exists(i in index_set(partitions)) (
      v1 in partitions[i] /\
      v2 in partitions[i] /\
      v1 != v2
  )
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
  % /\
  % var1 = 6
  % /\
  % var2 = 2
  /\
  in_same_partition(var1, var2, s)
;

output [
"s: " ++ show(s) ++ "\n"++
"var1: " ++ show(var1) ++ "\n"++
"var2: " ++ show(var2) ++ "\n"
];
