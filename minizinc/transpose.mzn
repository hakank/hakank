% 
% Implementation of some matrix operations in MiniZinc.
% 
% - transpose
% - scalar_add
% - scalar_mult
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n;
array[1..n, 1..n] of var 1..10: x;
array[1..n, 1..n] of var 1..10: t;
array[1..n, 1..n] of var int: p;

%
% Transposes a matrix
%
predicate transpose(array[int, int] of var int: x, array[int, int] of var int: t) =
   forall(i in index_set_1of2(x), j in index_set_2of2(x) ) (
      t[j,i] = x[i,j]
   )
;

%
% Scalar matrix addition
%
predicate scalar_add(array[int, int] of var int: x, array[int, int] of var int: y, array[int, int] of var int: res) =
   forall(i in index_set_1of2(x), j in index_set_2of2(x) ) (
      res[i,j] = x[i,j] + y[i,j]
   )
;

%
% Scalar matrix multiplication
%
predicate scalar_mult(array[int, int] of var int: x, array[int, int] of var int: y, array[int, int] of var int: res) =
   forall(i in index_set_1of2(x), j in index_set_2of2(x) ) (
      res[i,j] = x[i,j] * y[i,j]
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

%  cp2d(x,[1,1,1,1,
%        2,2,2,2,
%        3,3,3,3,
%        4,4,4,4])
%   /\
   transpose(x,t)
%   /\
%  scalar_add(x,t,p)
%   /\
%  cp2d(p, [|2,3,4,5,
%        |3,4,5,6,
%        |4,5,6,7,
%        |5,6,7,8|])
    /\
    scalar_add(x,t,p)
%    /\ 
%   cp2d(p, [|1, 2, 3, 4,
%         |2, 4, 6, 8,
%         |3, 6, 9, 12,
%         |4, 8, 12, 16|])
    /\
    cp2d(p, [|2, 3, 4, 5,
         |3, 4, 5, 6,
         |4, 5, 6, 7,
         |5, 6, 7, 8|])

;

output 
[ 
  "x: "
] ++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
] ++
[ "\nt: "] ++
[
  if j = 1 then "\n" else " " endif ++
    show(t[i,j])
  | i,j in 1..n
] ++
[ "\np: "] ++
[
  if j = 1 then "\n" else " " endif ++
    show(p[i,j])
  | i,j in 1..n
] ++
[ "\n" ]
;

%
% data
%
n = 4;

