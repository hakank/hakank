% 
% Kakuro puzzle in MiniZinc.
% 
% http://en.wikipedia.org/wiki/Kakuro
% """
% The object of the puzzle is to insert a digit from 1 to 9 inclusive 
% into each white cell such that the sum of the numbers in each entry 
% matches the clue associated with it and that no digit is duplicated in 
% any entry. It is that lack of duplication that makes creating Kakuro 
% puzzles with unique solutions possible, and which means solving a Kakuro 
% puzzle involves investigating combinations more, compared to Sudoku in 
% which the focus is on permutations. There is an unwritten rule for 
% making Kakuro puzzles that each clue must have at least two numbers 
% that add up to it. This is because including one number is mathematically 
% trivial when solving Kakuro puzzles; one can simply disregard the 
% number entirely and subtract it from the clue it indicates.
% """
%
% This model solves the problem at the Wikipedia page. 
% For a larger picture, see
% http://en.wikipedia.org/wiki/File:Kakuro_black_box.svg
%
% The solution:
%   9 7 0 0 8 7 9
%   8 9 0 8 9 5 7
%   6 8 5 9 7 0 0
%   0 6 1 0 2 6 0
%   0 0 4 6 1 3 2
%   8 9 3 1 0 1 4
%   3 1 2 0 0 2 1
%
% or rather
%
%   9 7 _ _ 8 7 9
%   8 9 _ 8 9 5 7
%   6 8 5 9 7 _ _
%   _ 6 1 _ 2 6 _
%   _ _ 4 6 1 3 2
%   8 9 3 1 _ 1 4
%   3 1 2 _ _ 2 1


%
%
% Also, see the related model:
%  - MiniZinc: http://www.hakank.org/comet/kakuro.mzn
% 
% Note: In this current model there is an alternative 
% way of representing the hints and the blanks,
% namely as segments in the two hint grids:
%   hints_row
%   hints_col
% And the blanks are in these grids as well.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 7;
int: B = -1; % Blank in the hint grids

%
% state the problem (without the operation)
%
% For a better view of the problem, see
%  http:%en.wikipedia.org/wiki/File:KenKenProblem.svg
%
int: num_row_hints = 12; % number of row hints
int: num_col_hints = 12; % number of column hints

% The numbers are the "hint segment" a hint belongs to.
% The blanks (B) are also in these grids.

% The row hints
array[1..n, 1..n] of int: hints_row = array2d(1..n, 1..n,
[
%  1  2  3  4  5  6  7
   1, 1, B, B, 2, 2, 2, % 1
   3, 3, B, 4, 4, 4, 4, % 2
   5, 5, 5, 5, 5, B, B, % 3
   B, 6, 6, B, 7, 7, B, % 4
   B, B, 8, 8, 8, 8, 8, % 5
   9, 9, 9, 9, B,10,10, % 6
  11,11,11, B, B,12,12, % 7
]);

% The column hints
array[1..n, 1..n] of int: hints_col = array2d(1..n, 1..n,
[
% 1  2  3  4  5  6  7
 13,15, B, B,20,21,23, % 1
 13,15, B,18,20,21,23, % 2
 13,15,17,18,20, B, B, % 3
  B,15,17, B,20,22, B, % 4
  B, B,17,19,20,22,24, % 5
 14,16,17,19, B,22,24, % 6
 14,16,17, B, B,22,24, % 7
]);

% sums for row hints
array[1..num_row_hints] of int: row_hint_sums = 
[ 16, 24, 17, 29, 35, 7, 8, 16, 21, 5, 6, 3];

% sums for column hints
array[1..num_col_hints] of int: col_hint_sums =
[ 23, 11, 30, 10, 15, 17, 7, 27, 12, 12, 16, 7];

%
% decision variables
%
array[1..n, 1..n] of var 0..9: x;


solve :: int_search([x[i,j] | i,j in 1..n], first_fail, indomain_max, complete) satisfy;


constraint

  % Handle blanks
  forall(i,j in 1..n) (
        if hints_row[i,j] = B then
           x[i,j] = 0
        else
           x[i,j] > 0
        endif
  )

  /\ % Rows
  forall(p in 1..num_row_hints) (
      row_hint_sums[p] = sum(i,j in 1..n where hints_row[i,j] = p) ( x[i,j])
      /\
      alldifferent([x[i,j] | i,j in 1..n where hints_row[i,j] = p])
  )

  /\ % Columns
  forall(p in 1..num_col_hints) (
      col_hint_sums[p] = sum(i,j in 1..n where hints_col[i,j] = p+num_row_hints) ( x[i,j])
      /\
      alldifferent([x[i,j] | i,j in 1..n where hints_col[i,j] = p+num_row_hints])
  )
;


output [
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
];