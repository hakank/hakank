% 
% Global constraint max_index (and the extension max_index_val) in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cmax_index.html
% """
% max_index​(MAX_INDEX,​VARIABLES)
% 
% Purpose
%
% MAX_INDEX is the index of the variables corresponding to the maximum 
% value of the collection of variables VARIABLES.
% 
% Example
%     (
%     3, <
%     index-1  var-3,
%     index-2  var-2,
%     index-3  var-7,
%     index-4  var-2,
%     index-5  var-6
%     >
%     )
%
% The attribute var=7 of the third item of the collection VARIABLES is 
% the maximum value over values 3,​2,​7,​2,​6. Consequently, the max_index 
% constraint holds since its first argument MAX_INDEX is set to 3.
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 5;
array[1..n] of var 1..7: variables;
var 1..n: max_ind;
var 1..7: max_val;


% I assume that the indices are ordered and unique
predicate max_index(var int: mi, array[int] of var int: x) =
  exists(i in index_set(x)) (
     x[i] = max(x) % maximum(x[i], x)
     /\
     mi = i 
  )
;

%
% An extension of max_index: also returns the maximum value
%
predicate max_index_val(var int: mi, array[int] of var int: x, var int: val) =
  exists(i in index_set(x)) (
     x[i] = max(x) % maximum(x[i], x)
     /\
     mi = i 
     /\
     val = x[i]
  )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


% solve satisfy;
solve :: int_search(variables ++ [max_ind, max_val], first_fail, indomain_min, complete) satisfy;

constraint
  cp1d(variables, [3,2,7,2,6])
  /\
  % max_index(max_ind, variables)
  max_index_val(max_ind, variables, max_val)
  
;

output [
  "variables: ", show(variables), "\n",
  "max_ind: ", show(max_ind), "\n",
  "max_val: ", show(max_val), "\n",
];
