% 
% Global constraint elements in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Celements.html
% """
% Constraint
%
%     elements(ITEMS,TABLE)
%
% Restriction(s)
%     required(ITEMS,[index,value])
%     ITEMS.index>=1
%     ITEMS.index<=|TABLE|
%     required(TABLE,[index,value])
%     TABLE.index>=1
%     TABLE.index<=|TABLE|
%     distinct(TABLE,index)
% Purpose
%
% All the items of ITEMS should be equal to one of the entries of the 
% table TABLE.
%
% Example
%     (
%     <index-4 value-9, index-1 value-6>,
%     <
%         index-1 value-6,
%         index-2 value-9,
%         index-3 value-2,
%         index-4 value-9
%     >
%     )
%
% The elements constraint holds since each item of its first argument ITEMS 
% corresponds to an item of the TABLE collection: the first item 
% <index-4 value-9> of ITEMS corresponds to the fourth item of TABLE, while 
% the second item <index-1 value-6> of ITEMS corresponds to the first 
% item of TABLE.
%
% Usage
%
% Used for replacing several element constraints sharing exactly the same table 
% by one single constraint.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 4; % length of table
int: m = 2; % length of items
array[1..n] of var 1..9: element_table;
array[1..m] of var 1..9: items;


predicate elements(array[int] of var int: items, array[int] of var int: elements_table) =
  forall(i in index_set(items)) (
    exists(j in index_set(element_table)) (
       items[i] = element_table[j]
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
  cp1d(items, [9,6])
  % items = [9,5] % false
  /\
  cp1d(element_table, [6,9,2,9])
  /\
  elements(items, element_table)
;


output
[
  "element_table:" ++ show(element_table) ++ "\n" ++
  "items:" ++ show(items) ++ "\n"
];
