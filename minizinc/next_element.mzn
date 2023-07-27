% 
% Global constraint next_element in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cnext_element.html
% """
% Constraint
%
%     next_element(THRESHOLD,INDEX,TABLE,VAL)
%
% Purpose
%
%     INDEX is the smallest entry of TABLE strictly greater than THRESHOLD 
%     containing value VAL.
%
% Example
%     (
%     2,3, <
%     index-1 value-1,
%     index-2 value-8,
%     index-3 value-9,
%     index-4 value-5,
%     index-5 value-9
%     >, 9
%     )
%
%     The next_element constraint holds since 3 is the smallest entry 
%     located after entry 2 that contains value 9.
%
% Usage
%
%     Originally introduced for modelling the fact that a nucleotide 
%     has to be consumed as soon as possible at cycle INDEX after a 
%     given cycle represented by variable THRESHOLD.
%
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 5;
int: m = 9;
array[1..n, 1..2] of var 1..m: table_vars;
var lb_array(table_vars)..ub_array(table_vars): ind;
var 0..m: threshold;
var lb_array(table_vars)..ub_array(table_vars): val;

%
% Note: I'm not sure I have understood this constraint correctly, 
%       especially the "smallest entry" part. I interpret this
%       just that ind > threshold, and not that ind must follow 
%       threshold immediatly in the indices of table.
%
predicate next_element(var int: threshold, 
                       var int: ind, 
                       array[int, 1..2] of var int: table_vars, 
                       var int: val) =
  ind > threshold
  /\
  all_different([table_vars[i,1] | i in index_set_1of2(table_vars)])

  /\  % ensure that threshold and ind are indices in table
  exists(k in index_set_1of2(table_vars)) (
      table_vars[k,1] = threshold
  )
  /\
  exists(k in index_set_1of2(table_vars)) (
     table_vars[k,1] = ind
     /\
     val = table_vars[k,2]
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

solve satisfy;

constraint
  threshold = 2 /\
  ind = 3 /\
  cp2d(table_vars, array2d(1..n, 1..2, [
          1,1,
          2,8,
          3,9,
          4,5,
          5,9
         ])) /\
  val = 9 /\
  next_element(threshold, ind, table_vars, val)
;

output
[
  "threshold: " ++ show(threshold) ++ "\n" ++
  "ind: " ++ show(ind) ++ "\n" ++
  "val: " ++ show(val) ++ "\n" 
];
