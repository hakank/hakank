% 
% Binary matrix to number in MiniZinc.
% 
% binmatrix2num converts a binary matrix to a list of numbers.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 5;
array[1..n, 1..n] of var 0..1: x;
array[1..n] of var 0..n: num_list;

%
% converts a binary matrix to a number array.
% Requires support of <-> (i.e. not IP models)
predicate binmatrix2num(array[int,int] of var int: x, array[int] of var int: nums) =

  forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
     nums[i] = j <-> x[i,j] = 1
  )
;

%
% For IP models
%
predicate binmatrix2num_ip(array[int,int] of var int: x, array[int] of var int: nums) =

  forall(i in index_set_1of2(x)) (
     nums[i] =  sum(j in index_set_2of2(x)) (j*x[i,j])
  )
  /\
  forall(i in index_set_1of2(x)) (
      sum(j in index_set_2of2(x)) (x[i,j]) = 1
  )
  /\
  forall(i in index_set_2of2(x)) (
      sum(j in index_set_1of2(x)) (x[i,j]) = 1
  )
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
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
%    cp2d(x,array2d(1..n, 1..n,  
%         [
%           0,0,1,0,0,
%           0,0,0,1,0,
%           0,1,0,0,0,
%           1,0,0,0,0,
%           0,0,0,0,1,
%         ]))
%    /\
   cp1d(num_list,[4,5,2,3,1])
   /\
   % binmatrix2num(x, num_list)
   binmatrix2num_ip(x, num_list)
;


output [ "num_list: ", show(num_list), "\n" ] ++
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n

] ++ ["\n"];