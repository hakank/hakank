% 
% Two Cube Calendar in MiniZinc.
% 
% Martin Gardner (April 1969):
% Construct two cubes which can be used as a calendar, i.e. together can 
% represent the numbers 01..31. One cube has the sides 1,2 and the other 3,4,5.

% Answer:
%  cube1 = [0,1,2,6,7,8]
%  cube2 = [0,1,2,3,4,5]


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

array[1..6] of var 0..9: cube1;
array[1..6] of var 0..9: cube2;
array[1..31, 1..2] of var 0..9: x;

predicate contains(var int: e, array[int] of var int: a) =
   exists(i in 1..length(a)) (
      a[i] = e
   )
;

predicate toNum(array[int] of var int: number, var int: num_sum, float: base) =
  let { 
     int: len = length(number) 
   }
   in
   num_sum = sum(i in 1..len) (
     ceil(pow(base, int2float(len-i))) * number[i]
   )
   /\ 
   forall(i in 1..len) (number[i] >= 0)
;


% solve satisfy;
solve :: int_search(cube1 ++ cube2 ++ [x[i,j] | i in 1..31, j in 1..2 ], 
        first_fail, indomain_min, complete) satisfy;


constraint
%  cube1 = [0,1,2,6,7,8]
%  /\
%  cube2 = [0,1,2,3,4,5]
%  /\
  contains(1, cube1) /\ contains(2, cube1) /\
  contains(3, cube2) /\ contains(4, cube2) /\ contains(5, cube2)
  /\
  forall(k in 1..31) (
    let {
      var 1..6: i,
      var 1..6: j,
      array[1..2] of var 0..9: a
    }
    in
    toNum(a, k, 10.0)
    /\
    x[k,1] = a[1]
    /\
    % special hack: instead of 9 we use 6
    ( 
     a[2] = 9 ->
     (
      (contains(6, cube1)  \/  contains(6, cube2) )
      /\
      x[k,2] = 6
     )
    )
    /\
    (
     a[2] != 9 <->
     (
      x[k, 2] = a[2]
      /\
      (
        10*cube1[i]  + cube2[j] = k
        /\
        x[k, 1] = cube1[i] /\ x[k, 2] = cube2[j]
      )
      \/
      (
        10*cube2[i] + cube1[j] = k
        /\
        x[k, 1] = cube2[i] /\ x[k, 2] = cube1[j]
      )
    )
   )
     
  )
  /\ % symmetry breaking and efficiency
  all_different(cube1) /\ increasing(cube1)
  /\
  all_different(cube2) /\ increasing(cube2)
  /\
  forall(i in 0..8) (
    % at_least(1, cube1 ++ cube2, i)
    count(t in cube1 ++ cube2)(t = i) >= 1
  )
;


output [
 "cube1: ", show(cube1), "\n",
 "cube2: ", show(cube2), "\n",
% "x: ", show(x), "\n",
] ++ 
[
 show(k) ++ ": " ++ show(x[k,1]) ++ show(x[k,2]) ++ "\n"
 | k in 1..31
];
