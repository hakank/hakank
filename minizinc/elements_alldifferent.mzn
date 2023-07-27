% 
% Global constraint elements_alldifferent in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Celements_alldifferent.html
% """
% Constraint
%
%  elements_alldifferent(ITEMS,TABLE)
% 
% Purpose
%
% All the items of the ITEMS collection should be equal to one of the entries 
% of the table TABLE and all the variables ITEMS.index should take 
% distinct values.
%
% Example
%     (
%     <
%         index-2 value-9,
%         index-1 value-6,
%         index-4 value-9,
%         index-3 value-2
%     >,
%     <
%         index-1 value-6,
%         index-2 value-9,
%         index-3 value-2,
%         index-4 value-9
%     >
%     )
%
% The elements_alldifferent constraint holds since, as depicted by 
% Figure 4.114.1 [see the referred page], there is a one to one 
% correspondence between the items of the ITEMS collection and the 
% items of the TABLE collection.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 4; % length of table
int: m = 4; % length of items
array[1..n, 1..2] of var 1..9: element_table;
array[1..m, 1..2] of var 1..9: items;

predicate elements_alldifferent(array[int,int] of var int: items, array[int,int] of var int: element_table) = 
  forall(i in index_set_1of2(items)) (
    exists(j in index_set_1of2(element_table)) (
       items[i,1] = element_table[j,1] /\
       items[i,2] = element_table[j,2]
    )
  )
  /\
  all_different([items[i,1] | i in index_set_1of2(items)])

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
  cp2d(items, array2d(1..m, 1..2, [
                      2,9,
                      1,6,
                      4,9,
                      3,2
                  ]))
  /\
  cp2d(element_table, array2d(1..n, 1..2, [ 
                  1,6,
                  2,9,
                  3,2,
                  4,9
                  ]))
  /\
  elements_alldifferent(items, element_table)


;

output
[
  "element_table:" ++ show(element_table) ++ "\n" ++
  "items: " ++ show(items) ++ "\n"
];
