% 
% Global constraint element_greatereq in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Celement_lesseq.html
% """
% Constraint
%
%     element_lesseq​(ITEM,​TABLE)​
% ...
%
% Purpose
%
%     ITEM.value is less than or equal to one of the entries (i.e., 
%     the value attribute) of the table TABLE.
%
% Example
%     (
%     <index-3 value-1>,
%     <
%     index-1 value-6,
%     index-2 value-9,
%     index-3 value-2,
%     index-4 value-9
%     >
%     )
%
%     The element_lesseq constraint holds since ITEM[1].value=1 is less than 
%     or equal to TABLE [ITEM[1].index].value=TABLE[3].value=2.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 4;
array[1..n] of var 1..9: element_table;
var 0..10: item;

predicate element_lesseq(var int: item, array[int] of var int: element_table) =
   exists(i in index_set(element_table)) (
       item <= element_table[i]
   )

;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint
   cp1d(element_table,[6,9,2,9])
   /\
   item = 1
   % item = 10 % this is false
   /\
   element_lesseq(item, element_table)
;


output
[
 "element_table:" ++ show(element_table) ++ "\n" ++
 "item: " ++ show(item)
];
