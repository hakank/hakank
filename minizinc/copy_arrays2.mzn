% 
% Copy arrays (functions) in MiniZinc 2.
% 
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 4;
array[1..n] of var 1..n: x;

array[1..n,1..2] of var 1..n: y;

%
% int
%
% % arrays of 1d where the first is fix int, the second is var int
predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 

% % arrays of 1d where both arguments are var int
predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


function array[int] of var int: cp1df(array[int] of var int: x) = 
   let {
      array[index_set(x)] of var int: y;
      constraint
        forall(i in index_set(x)) (
           y[i] = x[i]
        )
      ;
   }
   in y
;

function array[int] of var int: cp1df2(array[int] of var int: x) = 
   let {
      array[index_set(x)] of var int: y;
      constraint
        cp1d(x,y)
      ;
   }
   in y
;

% arrays of 2d where the first is fix int
predicate cp2d(array[int,int] of int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
      forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
     ) 
  )
; 

% % arrays of 2d where both arguments are var int
predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 

% function variant
function array[int,int] of var int: cp2d(array[int,int] of var int: x) =
    let {
       array[index_set_1of2(x), index_set_2of2(x)] of var int: y;
       constraint
       forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
          y[i,j] = x[i,j]
       );

    } in y
; 

%
% set of int
%
% first argument is set of fix int
predicate cp1d(array[int] of set of int: x, array[int] of var set of int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ))
; 

% both arguments are var set of int
predicate cp1d(array[int] of var set of int: x, array[int] of var set of int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ))
; 

predicate cp2d(array[int,int] of set of int: x, array[int,int] of var set of int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
    forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
        y[i,j] = x[i,j]
    ) 
  )
; 

predicate cp2d(array[int,int] of var set of int: x, array[int,int] of var set of int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
        y[i,j] = x[i,j]
     ) 
 )
; 




solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

   % 1d
   % cp1d([1,2,_,4], x)
   % cp1d([1,2,3,4], x)
   % x = cp1df([1,2,_,4])
   % x = cp1df2([1,2,3,4])
   % /\ increasing(x)
   x = [1,2,_,4]
   /\ 
 
   %% 2d
   %% These two variants works
   % cp2d(array2d(1..n, 1..2, 
   %    [1,2,3,4,
   %     4,3,2,1]), y)
   % y = cp2d(array2d(1..n, 1..2, 
   %   [1,2,3,4,
   %    4,3,2,1]))

   y = 
     [|1,2|3,4|4,3|2,1|]


   % don't work
   % y = 
   %   [1,2,3,4,
   %    4,3,2,1]

;


output
[
  "x: ", show(x), "\n", 
  "y: ", show(y)
];

