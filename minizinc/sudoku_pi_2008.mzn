% 
% Pi Day Sudoku 2008 in MiniZinc.
% 
% From Brainfreezepuzzles
% "Pi Day 2008"
% http://brainfreezepuzzles.com/main/piday2008.html
% The rules are in the PDF file:
% http://brainfreezepuzzles.com/main/files/pi_sudoku_2008.pdf
% """
% Rules: Each row, column, and jigsaw region must contain exactly 
% the first twelve digits of pi, including repeats: 3.14159265358. 
% Notice that each region will contain 
%    two 1's, 
%    two 3's, 
%    three 5's, 
%    and no 7's. 
% This puzzle is the most fun you'll ever have memorizing the 
% first twelve digits of π! 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: num_regions = 12; % number of regions
int: n = 12;           % number of rows and columns

% the problem as a n x n matrix
array[1..n, 1..n] of int: problem;

% decision variable
array[1..n, 1..n] of var 1..n: x;

% occurences: 314159265358
%                             1 2 3 4 5 6 7 8 9                              
array[1..9] of int: occ    = [2,1,2,1,3,1,0,1,1];
array[1..9] of 1..n: cover = [i | i in 1..9];

% solve satisfy;
solve :: int_search(
       [x[i,j] | i in 1..n, j in 1..n], 
       first_fail, 
       indomain_min, 
       complete) 
    satisfy;

% the hints
constraint
   forall(i,j in 1..n) (
      if problem[i,j] > 0 then
          x[i,j] = problem[i,j]
      else 
         true
      endif
   )
;

% GCC on rows and columns
constraint
  forall(i in 1..n) (
    global_cardinality([x[i,j] | j in 1..n], cover, occ) :: domain
    /\
    global_cardinality([x[j,i] | j in 1..n], cover, occ) :: domain
  )
;

%
% GCC on regions
%
constraint
  forall(k in 1..num_regions) (
    global_cardinality([x[i,j] | i,j in 1..n where regions[i,j] == k], cover, occ) :: domain
  )
;


%
% Given hints
%
problem = array2d(1..n, 1..n,
  [
    % 1  2  3  4  5  6  7  8  9  0  1  2
      3, 0, 0, 1, 5, 4, 0, 0, 1, 0, 9, 5, % 1
      0, 1, 0, 0, 3, 0, 0, 0, 0, 1, 3, 6, % 2
      0, 0, 4, 0, 0, 3, 0, 8, 0, 0, 2, 0, % 3
      5, 0, 0, 1, 0, 0, 9, 2, 5, 0, 0, 1, % 4
      0, 9, 0, 0, 5, 0, 0, 5, 0, 0, 0, 0, % 5
      5, 8, 1, 0, 0, 9, 0, 0, 3, 0, 6, 0, % 6
      0, 5, 0, 8, 0, 0, 2, 0, 0, 5, 5, 3, % 7
      0, 0, 0, 0, 5, 0, 0, 6, 0, 0, 1, 0, % 8
      2, 0, 0, 5, 1, 5, 0, 0, 5, 0, 0, 9, % 9
      0, 6, 0, 0, 4, 0, 1, 0, 0, 3, 0, 0, % 10
      1, 5, 1, 0, 0, 0, 0, 5, 0, 0, 5, 0, % 11
      5, 5, 0, 4, 0, 0, 3, 1, 6, 0, 0, 8, % 12
  ]);

%
% Regions:
% We code the cells as the region it belong
%
array[1..n, 1..n] of int: regions =
array2d(1..n, 1..n, 
[
   % 1  2  3  4  5  6  7  8  9  0  1  2
     1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, % 1
     1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, % 2
     1, 1, 4, 4, 4, 4, 5, 5, 5, 5, 3, 3, % 3
     1, 1, 4, 4, 4, 4, 5, 5, 5, 5, 3, 3, % 4
     1, 1, 4, 4, 4, 4, 5, 5, 5, 5, 3, 3, % 5
     6, 6, 6, 7, 7, 8, 8, 9, 9,10,10,10, % 6
     6, 6, 6, 7, 7, 8, 8, 9, 9,10,10,10, % 7
     6, 6, 6, 7, 7, 8, 8, 9, 9,10,10,10, % 8
     6, 6, 6, 7, 7, 8, 8, 9, 9,10,10,10, % 9
    11,11,11, 7, 7, 8, 8, 9, 9,12,12,12, % 10
    11,11,11, 7, 7, 8, 8, 9, 9,12,12,12, % 11
    11,11,11,11,11,11,12,12,12,12,12,12  % 12
]);



output [
  if j = 1 then "\n" else " " endif ++ 
    if fix(x[i,j]) < 10 then " " else "" endif ++
    show(x[i,j])
  | i,j in 1..n
] 
++ 
["\n"];