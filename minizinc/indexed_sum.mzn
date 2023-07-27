% 
% Global constraint indexed_sum in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cindexed_sum.html
% """
% indexed_sum​(ITEMS,​TABLE)
% 
% Purpose
%
% Given several items of the collection ITEMS (each of them having a specific 
% fixed index as well as a weight that may be negative or positive), and a 
% table TABLE (each entry of TABLE corresponding to a summation variable), 
% assign each item to an entry of TABLE so that the sum of the weights of 
% the items assigned to that entry is equal to the corresponding summation 
% variable.
% 
% Example
%     (
%     <index-2 weight--4,​index-0 weight-6,​index-2 weight-1>,
%     <index-0 summation-6,​index-1 summation-0,​index-2 summation--3>
%     )
%
% The indexed_sum constraint holds since the summation variables associated 
% with each entry of TABLE are equal to the sum of the weights of the 
% items assigned to the corresponding entry:
% 
%  * TABLE[1].summation=ITEMS[2].weight=6 (since TABLE[1].index=ITEMS[2].index=0),
%  * TABLE[2].summation=0 (since TABLE[2].index=1 does not occur as a value 
%    of the index attribute of an item of ITEMS),
%  * TABLE[3].summation=ITEMS[1].weight+ITEMS[3].weight=-4+1=-3 (since 
%    TABLE[3].index=ITEMS[1].index=ITEMS[3].index=2).
%
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 3;
int: num_rows = 3;
array[1..num_rows, 1..2] of -10..10: items;
array[1..n] of var -10..10: xtable; 

%
% indexed_sum
% 
% Note: items is be par int (not var int)
%
predicate indexed_sum(array[int, 1..2] of int: items, 
                      array[int] of var int: xtable) =

   forall(i in index_set_1of2(items)) (
      xtable[i] = sum(j in index_set(xtable) where items[j, 1] = i) (
        items[j, 2]
      )
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint
   cp1d(xtable, [6, 0, -3])
   /\
   indexed_sum(items, xtable)
;

%
% data
%
% Note: I changed the item index to 1-based
items = array2d(1..num_rows, 1..2, 
      [
         3, -4,
         1, 6,
         3, 1
      ]) 
;

output
[
  "xtable: " ++ show(xtable) ++ "\n" ++
  "items: " ++ show(items) ++ "\n" 
];
