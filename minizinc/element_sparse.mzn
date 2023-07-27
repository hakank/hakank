% 
% Global constraint element_sparse in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Celement_sparse.html
% """
%  Constraint
%
%     element_sparse(ITEM,TABLE,DEFAULT)
% 
% Purpose
%
%     ITEM.value is equal to one of the entries of the table TABLE or to the default value 
%     DEFAULT if the entry ITEM.index does not exist in TABLE.
%
% Example
%     (
%     <index-2 value-5>,
%     <
%         index-1 value-6,
%         index-2 value-5,
%         index-4 value-2,
%         index-8 value-9
%     >,5
%     )
%
%     The element_sparse constraint holds since its first argument ITEM corresponds to the 
%     second item of the TABLE collection.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 4;
array[1..n,1..2] of var 0..9: element_table;
array[1..2] of var 0..9: item;
var 0..9: item_default; 
var 0..9: result;

%
% There is no meaning of setting
%      item[2] = item_default 
% when there is no item in the element table (since this is always false).
% Therefore I added the last argument (item_value) as the result.
%
predicate element_sparse(array[int] of var int: item, array[int,1..2] of var int: element_table, var int: item_default, var int: item_value) =

      exists(i in index_set_1of2(element_table)) (
         item[1] = element_table[i,1] /\
         item[2] = element_table[i,2] /\
         item_value = element_table[i,2]
      )
      \/
      item_value = item_default


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
   cp1d(item,[2,5])
   % item = [2,2] % don't exists: result -> item_default
   /\
   cp2d(element_table, array2d(1..n,1..2, [
                          1,2,
                          2,5,
                          4,2,
                          8,9
                   ]))
   /\
   item_default = 5
   /\
   element_sparse(item, element_table, item_default, result)
;


output
[
  "element_table:" ++ show(element_table) ++ "\n" ++
  "item: " ++ show(item) ++ "\n" ++
  "item_default: " ++ show(item_default) ++ "\n" ++
  "result: " ++ show(result)
];
