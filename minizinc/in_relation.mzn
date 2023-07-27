% 
% Global constraint in_relation in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cin_relation.html
% """
% in_relation​(VARIABLES,​TUPLES_OF_VALS)
%
% Purpose
%
% Enforce the tuple of variables VARIABLES to take its value out of a set 
% of tuples of values TUPLES_OF_VALS. The value of a tuple of variables 
% <V1, V2, ..., Vn> is a tuple of values <U1, U2, ..., Un> if and only 
% if V1=U1/\V2=U2/\.../\Vn=Un.
%
% Example
%     (
%     <5, 3, 3>,
%     <
%     tuple-<5, 2, 3>,
%     tuple-<5, 2, 6>,
%     tuple-<5, 3, 3>
%     >
%     )
%
% The in_relation constraint holds since its first argument <5, 3, 3> corresponds to the 
% third item of the collection of tuples TUPLES_OF_VALS.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 4;
int: m = 3;
array[1..n, 1..m] of var 1..6: arr;
array[1..m] of var 1..6: v1;
% var 1..n: pos; % for in_relation_index

%
% in_relation(VARIABLES, TUPLES_OF_VALS)
%
predicate in_relation(array[int] of var int: v, array[int, int] of var int: a) =
      exists(i in index_set_1of2(a)) (
      forall(j in index_set(v)) (
         a[i,j] = v[j]
      ) 
    )
;


%
% adding an index parameter: in which position is v?
% This is in effect the same as the global constraint cond_lex_cost
%
predicate in_relation_index(array[int] of var int: v, array[int, int] of var int: a, var int: ix) =
      exists(i in index_set_1of2(a)) (
      ix = i
      /\
      forall(j in index_set(v)) (
         a[i,j] = v[j]
      ) 
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


solve satisfy;

constraint
    cp1d(v1, [5,3,3])
    /\
    cp2d(arr, [|5, 2, 3,
           |5, 2, 6,
           |5, 3, 3
           |1, 3, 2|])
    /\
    in_relation(v1, arr)
    % in_relation_index(v1, arr, pos)
;


output
[
  "v1: " ++ show(v1)
];
