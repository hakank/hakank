% 
% Global constraint min_index (and the extension min_index_val) in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cmin_index.html
% """
% min_index​(MIN_INDEX,​VARIABLES)​
% 
% Purpose
% 
% MIN_INDEX is the index of the variables corresponding to the minimum 
% value of the collection of variables VARIABLES.

% Example
%     (
%     2,​<
%     index−1	var-3,
%     index-2	var-2,
%     index-3	var-7,
%     index-4	var-2,
%     index-5	var-6
%     >
%     )
%     (
%     4,​<
%     index-1	var-3,
%     index-2	var-2,
%     index-3	var-7,
%     index-4	var-2,
%     index-5	var-6
%     >
%     )
%
% The attribute var=2 of the second and fourth items of the collection 
% VARIABLES is the minimum value over values 3,​2,​7,​2,​6. Consequently, 
% both min_index constraints hold since their first arguments MIN_INDEX 
% are respectively set to 2 and 4.
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 5;
array[1..n] of var 1..7: variables;
var 1..n: min_ind;
var 1..7: min_val;

% I assume that the indices are ordered and unique
predicate min_index(var int: mi, array[int] of var int: x) =
  exists(i in index_set(x)) (
     x[i] = min(x) % minimum(x[i], x)
     /\
     mi = i 
  )
;

%
% An extension of max_index: also returns the maximum value
%
predicate min_index_val(var int: mi, array[int] of var int: x, var int: val) =
  exists(i in index_set(x)) (
     x[i] = min(x) % minimum(x[i], x)
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
solve :: int_search(variables ++ [min_ind, min_val], 
        first_fail, indomain_min, complete) satisfy;

constraint
  cp1d(variables, [3,2,7,2,6])
  /\
  % min_index(max_ind, variables)
  min_index_val(min_ind, variables, min_val)
  
;

output [
  "variables: ", show(variables), "\n",
  "min_ind: ", show(min_ind), "\n",
  "min_val: ", show(min_val), "\n",
];
