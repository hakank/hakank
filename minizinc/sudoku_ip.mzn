% 
% Sudoku, integer programming model in MiniZinc.
% 
% From GLPK:s example sudoku.mod:
% """
% SUDOKU, Number Placement Puzzle */
%
% Written in GNU MathProg by Andrew Makhorin <mao@mai2.rcnet.ru> */
%
% Sudoku, also known as Number Place, is a logic-based placement
% puzzle. The aim of the canonical puzzle is to enter a numerical
% digit from 1 through 9 in each cell of a 9x9 grid made up of 3x3
% subgrids (called "regions"), starting with various digits given in
% some cells (the "givens"). Each row, column, and region must contain
% only one instance of each numeral.
%
% Example:
%
% +-------+-------+-------+
% | 5 3 . | . 7 . | . . . |
% | 6 . . | 1 9 5 | . . . |
% | . 9 8 | . . . | . 6 . |
% +-------+-------+-------+
% | 8 . . | . 6 . | . . 3 |
% | 4 . . | 8 . 3 | . . 1 |
% | 7 . . | . 2 . | . . 6 |
% +-------+-------+-------+
% | . 6 . | . . . | 2 8 . |
% | . . . | 4 1 9 | . . 5 |
% | . . . | . 8 . | . 7 9 |
% +-------+-------+-------+
%
% (From Wikipedia, the free encyclopedia.) */
% """

% Note. The answer is:
%  +-------+-------+-------+
%  | 5 3 4 | 6 7 8 | 9 1 2 |
%  | 6 7 2 | 1 9 5 | 3 4 8 |
%  | 1 9 8 | 3 4 2 | 5 6 7 |
%  +-------+-------+-------+
%  | 8 5 9 | 7 6 1 | 4 2 3 |
%  | 4 2 6 | 8 5 3 | 7 9 1 |
%  | 7 1 3 | 9 2 4 | 8 5 6 |
%  +-------+-------+-------+
%  | 9 6 1 | 5 3 7 | 2 8 4 |
%  | 2 8 7 | 4 1 9 | 6 3 5 |
%  | 3 4 5 | 2 8 6 | 1 7 9 |
%  +-------+-------+-------+


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% the "givens" 
array[1..9,1..9] of 0..9: givens;

% x[i,j,k] = 1 means cell [i,j] is assigned number k 
array[1..9,1..9,1..9] of var 0..1: x;

% for the output
array[1..9, 1..9] of var 1..9: sol;

% there is no need for an objective function here 
solve satisfy;

constraint

   % assign pre-defined numbers using the "givens" 
   forall(i in 1..9, j in 1..9, k in 1..9 where givens[i,j] != 0) (
     x[i,j,k] = if givens[i,j] = k then 1 else 0 endif
   )
   /\
   % each cell must be assigned exactly one number 
   forall(i in 1..9, j in 1..9) (sum(k in 1..9) (x[i,j,k]) = 1)

   /\
   % cells in the same row must be assigned distinct numbers 
   forall(i in 1..9, k in 1..9) (sum(j in 1..9) (x[i,j,k]) = 1)

   /\
   % cells in the same column must be assigned distinct numbers 
   forall(j in 1..9, k in 1..9) (sum(i in 1..9) (x[i,j,k]) = 1)

   /\
   % cells in the same region must be assigned distinct numbers 
   forall(I in {1,4,7}, J in {1,4,7}, k in 1..9) (
     sum(i in I..I+2, j in J..J+2) (x[i,j,k]) = 1
   )

   /\ % the solution
   forall(i,j in 1..9) (
      sol[i,j] = sum(k in 1..9) (x[i,j,k] * k) 
   )
;

output [
  if j = 1 then "\n" else " " endif ++
    show(sol[i,j])
  | i,j in 1..9
] ++ ["\n"];


%
% data
%
% These data correspond to the example above. 
% 

givens = array2d(1..9, 1..9, 
[
5,3,0,0,7,0,0,0,0,
6,0,0,1,9,5,0,0,0,
0,9,8,0,0,0,0,6,0,
8,0,0,0,6,0,0,0,3,
4,0,0,8,0,3,0,0,1,
7,0,0,0,2,0,0,0,6,
0,6,0,0,0,0,2,8,0,
0,0,0,4,1,9,0,0,5,
0,0,0,0,8,0,0,7,9
]);



