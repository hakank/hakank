% 
% Killer Sudoku in MiniZinc.
% 
%   Killer sudoku in Comet.

%   http://en.wikipedia.org/wiki/Killer_Sudoku
%   """
%   Killer sudoku (also killer su doku, sumdoku, sum doku, addoku, or 
%   samunamupure) is a puzzle that combines elements of sudoku and kakuro. 
%   Despite the name, the simpler killer sudokus can be easier to solve 
%   than regular sudokus, depending on the solver's skill at mental arithmetic; 
%   the hardest ones, however, can take hours to crack.

%   ...

%   The objective is to fill the grid with numbers from 1 to 9 in a way that 
%   the following conditions are met:

%     * Each row, column, and nonet contains each number exactly once.
%     * The sum of all numbers in a cage must match the small number printed 
%       in its corner.
%     * No number appears more than once in a cage. (This is the standard rule 
%       for killer sudokus, and implies that no cage can include more 
%       than 9 cells.)

%   In 'Killer X', an additional rule is that each of the long diagonals 
%   contains each number once.
%   """

%   Here we solve the problem from the Wikipedia page, also shown here
%   http://en.wikipedia.org/wiki/File:Killersudoku_color.svg

%   Note, this model is based on the generalized KenKen model: 
%   http://www.hakank.org/comet/kenken2.co
%   Killer Sudoku is simpler in that the only mathematical operation is 
%   summation.

%   The output is:
%     2 1 5 6 4 7 3 9 8
%     3 6 8 9 5 2 1 7 4
%     7 9 4 3 8 1 6 5 2
%     5 8 6 2 7 4 9 3 1
%     1 4 2 5 9 3 8 6 7
%     9 7 3 8 1 6 4 2 5
%     8 2 1 7 3 9 5 4 6
%     6 5 9 4 2 8 7 1 3
%     4 3 7 1 6 5 2 8 9
%
% Also, see the following model:
%  MiniZinc: http://www.hakank.org/minizinc/killer_sudoku.mzn


% Note: In this current model there is an alternative 
% way of representing the hints, namely as segments in the 
% segment grid.


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 9;
array[1..n, 1..n] of var 1..9: x;

% For a better view of the problem, see
%  http://en.wikipedia.org/wiki/File:Killersudoku_color.svg
%

%
% segments
%
int: num_segments = 29; % number of segments

array[1..n, 1..n] of int: segments =
array2d(1..n, 1..n,
[
  1, 1, 2, 2, 2, 3, 4, 5, 6, % 1
  7, 7, 8, 8, 3, 3, 4, 5, 6, % 2
  7, 7, 9, 9, 3,10,11,11, 6, % 3
 13,14,14, 9,15,10,11,12, 6, % 4
 13,16,16,17,15,10,12,12,18, % 5
 19,16,20,17,15,21,22,22,18, % 6
 19,20,20,17,23,21,21,24,24, % 7
 19,25,26,23,23,27,27,24,24, % 8
 19,25,26,23,28,28,28,29,29, % 9
]);

array[1..num_segments] of int: segment_sums =
[
   3, % 1
  15, % 2
  22, % 3
   4, % 4
  16, % 5
  15, % 6
  25, % 7
  17, % 8
   9, % 9
   8, % 10
  20, % 11
  17, % 12
   6, % 13
  14, % 14
  17, % 15
  13, % 16
  20, % 17
  12, % 18
  27, % 19
   6, % 20
  20, % 21
   6, % 22
  10, % 23
  14, % 24
   8, % 25
  16, % 26
  15, % 27
  13, % 28
  17  % 29
];

% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n], first_fail, indomain_min, complete) satisfy;

% Standard Sudoku constraints
constraint
  % rows and columns
  forall(i in 1..n) (
     all_different([x[i,j] | j in 1..n]) /\
     all_different([x[j,i] | j in 1..n])
  )
  /\ % blocks
  forall(i in 0..2,j in 0..2) (
    all_different([x[r,c] | r in i*3+1..i*3+3, c in j*3+1..j*3+3] )
  )
;

% Handle the segments
constraint
  forall(p in 1..num_segments) (
    segment_sums[p] = sum([x[i,j] | i,j in 1..n where segments[i,j] = p])
  )
;

output [
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
];