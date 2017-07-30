% 
% Killer Sudoku in MiniZinc.
% 

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
% Also, see the following models:
%  Comet: http://www.hakank.org/comet/killer_sudoku.co
%  MiniZinc: http://www.hakank.org/minizinc/kenken2.mzn
%  MiniZinc: http://www.hakank.org/minizinc/kakuro.mzn

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 9;
array[1..n, 1..n] of var 1..9: x;

%
% state the problem
%
% For a better view of the problem, see
%  http://en.wikipedia.org/wiki/File:Killersudoku_color.svg
%
int: num_p = 29; % number of segments
int: num_hints = 4;  % number of hints per segments (that's max number of hints)
int: max_val = 100;
array[1..num_p, 1..2*num_hints+1] of 0..max_val: P =
  array2d(1..num_p, 1..2*num_hints+1, [
    1,1,  1,2, 0,0, 0,0,   3,
    1,3,  1,4, 1,5, 0,0,  15,
    1,6,  2,5, 2,6, 3,5,  22,
    1,7,  2,7, 0,0, 0,0,   4,
    1,8,  2,8, 0,0, 0,0,  16,
    1,9,  2,9, 3,9, 4,9,  15,
    2,1,  2,2, 3,1, 3,2,  25,
    2,3,  2,4, 0,0, 0,0,  17,
    3,3,  3,4, 4,4, 0,0,   9,
    3,6,  4,6, 5,6, 0,0,   8,
    3,7,  3,8, 4,7, 0,0,  20,
    4,1,  5,1, 0,0, 0,0,   6,
    4,2,  4,3, 0,0, 0,0,  14,
    4,5,  5,5, 6,5, 0,0,  17,
    4,8,  5,7, 5,8, 0,0,  17,
    5,2,  5,3, 6,2, 0,0,  13,
    5,4,  6,4, 7,4, 0,0,  20,
    5,9,  6,9, 0,0, 0,0,  12,
    6,1,  7,1, 8,1, 9,1,  27,
    6,3,  7,2, 7,3, 0,0,   6,
    6,6,  7,6, 7,7, 0,0,  20,
    6,7,  6,8, 0,0, 0,0,   6,
    7,5,  8,4, 8,5, 9,4,  10,
    7,8,  7,9, 8,8, 8,9,  14,
    8,2,  9,2, 0,0, 0,0,   8,
    8,3,  9,3, 0,0, 0,0,  16,
    8,6,  8,7, 0,0, 0,0,  15,
    9,5,  9,6, 9,7, 0,0,  13,
    9,8,  9,9, 0,0, 0,0,  17
   ]);


% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n], first_fail, indomain_min, complete) satisfy;

constraint
 
  forall(i in 1..n) (
        all_different([x[i,j] | j in 1..n]) /\
        all_different([x[j,i] | j in 1..n])
  )
  /\
  forall(i in 0..2,j in 0..2) (
    all_different([x[r,c] | r in i*3+1..i*3+3, c in j*3+1..j*3+3] )
  )
  /\ % calculate the hints
  forall(p in 1..num_p) (
     sum(i in 1..num_hints where P[p,2*(i-1)+1] > 0) (x[  P[p, 2*(i-1)+1], P[p,2*(i-1)+2]  ]) = P[p, 2*num_hints+1]
  )
;

output [
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
];