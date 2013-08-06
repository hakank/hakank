% 
% Global constraint allperm in MiniZinc.
% 
% From Global Constraint Catalogue
% """
% Given a matrix of domain variables, enforces that the first row is 
% lexicographically less than or equal to all permutations of all other rows.
% 
% Example
% (
%  <
%   vec-<1, 2, 3>,
%   vec-<3, 1, 2>
%  >
% )
%
% The allperm constraint holds since vector <1, 2, 3> is lexicographically 
% less than or equal to all the permutations of vector <3, 1, 2> 
% (i.e., <1, 2, 3>, <1, 3, 2>, <2, 1, 3>, <2, 3, 1>, <3, 1, 2>, <3, 2, 1>).
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n = 3;
int: m = 4; % number of rows
array[1..m, 1..n] of var 1..10: x;

%
% allperm
% 
% Simple version: 
% For all rows except the first:
%   Check that the sorted i'th row is the same as the first row.
%
predicate allperm(array[int, int] of var int: x) =
    let {
       int: lbx1 = min(index_set_1of2(x)),
       int: ubx1 = max(index_set_1of2(x)),
       int: lbx2 = min(index_set_2of2(x)),
       int: ubx2 = max(index_set_2of2(x)),
       array[lbx2..ubx2] of var lb_array(x)..ub_array(x): first_row = [x[1, j] | j in lbx2..ubx2]
    }
    in
    increasing(first_row) % assure that the first row is sorted
    /\
    forall(i in lbx1+1..ubx1) (
       let {
           array[lbx2..ubx2] of var lb_array(x)..ub_array(x): tmp
       }
       in
       sort([x[i, j] | j in lbx2..ubx2], tmp)
       /\
       cp1d(first_row, tmp)
       % first_row = tmp

    )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
       x[i] = y[i]
   )
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

output [
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i in 1..m, j in 1..n
] ++ ["\n"];

constraint
   cp2d(array2d(1..m, 1..n, [
      % _,_,_,
      1,2,3,
      3,1,2,
      2,3,1,
      2,1,3  
      ]), x)
    /\
   allperm(x)
;

