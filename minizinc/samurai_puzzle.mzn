% 
% Samuraï puzzle in MiniZinc.
% 
% From Constraint 2011 exam (Polytechnique)
% via Nicolas Beldiceanu.
% 
% For every sqrt(n) by sqrt(n) sub square there should be 
% exacly one 1, also there should be just one 1 on 
% each line/column.

% Here are all 16 possible solutions for n=4
% [0,2,1,3],
% [0,2,3,1],
% [0,3,1,2],
% [0,3,2,1],
% [1,2,0,3],
% [1,2,3,0],
% [1,3,0,2],
% [1,3,2,0],
% [2,0,1,3],
% [2,0,3,1],
% [2,1,0,3],
% [2,1,3,0],
% [3,0,1,2],
% [3,0,2,1],
% [3,1,0,2],
% [3,1,2,0]]

% Number of solutions for n
%  n  #solutions
%  -------------
%  1  1
%  4  16
%  9  46656
%  25 (too many solutions...)
%

% It seems to be http://oeis.org/A185141
% 1, 16, 46656, 110075314176, 619173642240000000000,...
%  (n!)^(2n)
% a(n) is the number of "templates", or ways of placing a single digit within an 
% n^2*n^2 Sudoku puzzle so that all rows, columns, and n*n blocks have exactly one 
% copy of the digit.
% Reference: G. Dahl, "Permutation matrices related to Sudoku, Linear Algebra and its 
% Applications", 430 (2001), 2457-2463.

% Number of solutions with the alldifferent_interval(x, m):
% n  #solutions
% -------------
% 1  1
% 4  8
% 9  1296
% 25 too many...


% Number of solutions with alldifferent_interval2(x, m)
% n  #solutions
% -------------
% 1  1
% 4  2
% 9  2000
% 25 too many...
% 

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 4; % must be a square
int: m = ceil(sqrt(int2float(n)));

% decision variables
array[1..n] of var 1..n: x;
array[1..n, 1..n] of var 0..1: y; % 0/1 version of the grid

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

%
% alldifferent_interval(x, m)
%
% which requires that the differences of all elements in the mxm 
% subsquares should be >= m
%
% For n=4 it gives these 8 solutions
% x: [0, 2, 1, 3]
% x: [0, 2, 3, 1]
% x: [1, 3, 0, 2]
% x: [1, 3, 2, 0]
% x: [2, 0, 1, 3]
% x: [2, 0, 3, 1]
% x: [3, 1, 0, 2]
% x: [3, 1, 2, 0]
% 
predicate alldifferent_interval(array[int] of var int: x, int: m) =
  forall(i in 0..m-1) (
    forall(j,k in i*m+1..i*m+m where j < k) (
      abs(x[j]-x[k]) >= m
    )
  )
;

%
% alldifferent_interval2(x, m)
%
% A variant where the difference of all _neighboring_ elements 
% must be >= m.
%
% For n=4 it gives these two solutions:
% x: [1, 3, 0, 2]
% x: [2, 0, 3, 1]
%
predicate alldifferent_interval2(array[int] of var int: x, int: m) =
  forall(i in 1..n-1) (
    abs(x[i]-x[i+1]) >= m
  )
;

constraint

  alldifferent(x)

  /\ % channeling between x and y
  forall(i, j in 1..n) (
    y[i,j] = 1 <-> x[i] = j
  )
  /\ % each sub square has just one 1 (the rest is 0's)
  forall(i in 0..m-1,j in 0..m-1) (
    sum([y[r,c] | r in i*m+1..i*m+m, c in j*m+1..j*m+m]) = 1
  )

  % /\ alldifferent_interval(x, m)
  % /\ alldifferent_interval2(x, m)
;

output [
 "x: " ++ show(x)
]
++
[
  if j = 1 then "\n" else " " endif ++
    if fix(x[i]) = j then 
      show(j) 
    else 
      show("_")
    endif
  ++
  if j mod m = 0 then " " else "" endif ++
  if i mod m = 0 /\ j = n then "\n" else "" endif

  | i, j  in 1..n,
]
++
[
  if j = 1 then "\n" else " " endif ++
    show(y[i,j]) ++
  if j mod m = 0 then " " else "" endif ++
  if i mod m = 0 /\ j = n then "\n" else "" endif
  | i, j  in 1..n,
]
;

