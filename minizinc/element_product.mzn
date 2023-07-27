% 
% Global constraint in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Celement_product.html
% """
% Constraint
%
%     element_product(Y, TABLE, X, Z)
% 
% Purpose
%
%     Z is equal to the Yth item of TABLE multiplied by X.
%
% Example
%     ​(3,​<6, 9, 2, 9>, 5, 10)
%
%     The element_product constraint holds since its fourth argument Z=10 
%     is equal to the 3th (Y=3) item of the collection <6, 9, 2, 9> 
%     multiplied by X=5.
%
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n = 4;
array[1..n] of var 1..9: element_table;
var int: x;
var int: y;
var int: z;

predicate element_product(var int: y, 
                        array[int] of var int: element_table,
                        var int: x, 
                        var int: z) =
    z = element_table[y] * x
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 


solve satisfy;

constraint
   cp1d(element_table,[6,9,2,9])
   /\
   x = 5
   /\
   y = 3
   /\
   element_product(y,element_table,x,z)
;


output
[
  "element_table: " ++ show(element_table) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" ++
  "y: " ++ show(y) ++ "\n" ++
  "z: " ++ show(z) ++ "\n" 
];
