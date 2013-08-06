% 
% Converting a binary matrix to/from an array in MiniZinc.
% 
 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 


int: n = 4; 
int: m = 4;
array[1..n, 1..m] of var 0..1: x; % the binary matrix
array[1..n] of var 1..n: a;       % the array to convert into

%
% binary matrix  <-> array with the corresponding value
% (dual direction)
%
predicate bin2array(array[int,int] of var 0..1: x, array[int] of var int: a) =

  forall(i in index_set_1of2(x)) (
    a[i] = sum(j in index_set_2of2(x)) (j*x[i,j])
    /\
    sum(j in index_set_2of2(x)) (x[i,j]) = 1
  )
;

predicate cp2d(array[int,int] of int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
      forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
     ) 
  )
; 


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
   cp2d(array2d(1..n, 1..m, 
        [ 0,0,1,0,
          0,1,0,0,
          0,0,0,1,
          1,0,0,0]),x)
    /\
   bin2array(x,a)
%   /\
%   a =  [3,2,4,1]
;

output [
  "a: ", show(a), "\n",
  "x: ",
] ++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i in 1..n, j in 1..m

] ++ ["\n"];
