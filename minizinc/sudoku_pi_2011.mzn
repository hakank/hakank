% 
% Pi Day Sudoku 2011 in MiniZinc.
% 
% From Brainfreezepuzzles
% "Pi Day 2011"
% http://brainfreezepuzzles.com/main/piday2011.html
% The rules are in the PDF file:
% http://brainfreezepuzzles.com/main/puzzles/Brainfreeze_PiDay2011.pdf
% """
% Rules: This pie has 12 wedges. Fill in the grid so that the numbers 1-12 
% appear exactly once in each region of the pie. There are three types of 
% regions in this pie, each containing 12 cells. They are: 
%   each of the six rings going all the way around the pie; 
%   each of the six pairs of opposite wedges; 
%   and each of the six adjacent pairs of wedges of the same color.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: m = 6;  % number of cycles (rows)
int: n = 12; % number of slices (wedges, columns)

% the problem as a m x n matrix
array[1..m, 1..n] of int: problem;

% decision variable
array[1..m, 1..n] of var 1..n: x;


% solve satisfy;
solve :: int_search([x[i,j] | i in 1..m, j in 1..n], first_fail, indomain_min, complete) satisfy;

% the hints
constraint
   forall(i in 1..m, j in 1..n) (
      if problem[i,j] > 0 then
          x[i,j] = problem[i,j]
      else 
         true
      endif
   )

;

% all different of rows (circles)
constraint
  forall(i in 1..m) (
    alldifferent([x[i,j] | j in 1..n]) %:: domain
  )
;

% alldifferent on neighbour wedges
constraint
  forall(j in 2..10 where j mod 2 = 0) (
    alldifferent([ x[i,j] | i in 1..m] ++
                 [ x[i,j+1] | i in 1..m]) %:: domain
      
  )
  /\ % 1 <-> 12
  alldifferent([ x[i,1] | i in 1..m] ++
               [ x[i,12] | i in 1..m]) %:: domain
;

% alldifferent on opposite wedges
constraint 
  forall(j in 1..m) (
    alldifferent([ x[i,j] | i in 1..m] ++
                 [ x[i,j+m] | i in 1..m]) %:: domain
  
  )
;


% We follow the clock numbering for the representation,
% Column 1 is the one o'clock wedge, and then clockwise...
% 
% Row 1 is the outer circle, row 2 the next, etc.
problem = array2d(1..m, 1..n,
  [
    % 1  2  3  4  5  6  7  8  9  0  1  2
      0, 0, 0, 0, 4, 9, 0, 0, 0, 0,11, 1, % 1
     12, 0, 0, 0, 7, 6, 8, 0, 0, 0, 9, 5, % 2
      9,10, 0, 0, 0, 0, 4,12, 0, 0, 0, 0, % 3
      3, 4, 2, 0, 0, 0, 1, 6,11, 0, 0, 0, % 4
      0, 0, 6,12, 0, 0, 0, 0, 1, 7, 0, 0, % 5
      0, 0, 9, 1, 5, 0, 0, 0, 4, 2, 3, 0, % 6
  ]);


output [
  if j = 1 then "\n" else " " endif ++ 
    if fix(x[i,j]) < 10 then " " else "" endif ++
    show(x[i,j])
  | i in 1..m, j in 1..n

] ++ ["\n"];