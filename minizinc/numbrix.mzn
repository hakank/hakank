% 
% Numbrix, Number Placement Puzzle in MiniZinc.
% 
% From GLPK example numbrix.mod
% """
% /* Written in GNU MathProg by Robert Wood <rwood@targus.com>  */
%
% /* Numbrix is a logic-based number-placement puzzle.[1]
%  * The objective is to fill the grid so that each cell contains
%  * digits in sequential order taking a horizontal or vertical
%  * path; diagonal paths are not allowed. The puzzle setter
%  * provides a grid often with the outer most cells completed.
%  *
%  * Completed Numbrix puzzles are usually a square of numbers
%  * in order from 1 to 64 (8x8 grid) or from 1 to 81 (9x9 grid),
%  * following a continuous path in sequence.
%  *
%  * The modern puzzle was invented by Marilyn vos Savant in 2008
%  * and published by Parade Magazine under the name "Numbrix",
%  * near her weekly Ask Marilyn article.
%  *
%  *    http://en.wikipedia.org/wiki/Numbrix  */
% """

% This is a huge MIP model.
%
% Note: Flattening in MiniZinc 2.0 takes about 21s, and about 6s in MiniZinc 1.6.
% Using the no-optimize flag shortens this time marginally.
%
% Solving it takes about 0.09s. 
%

%
% Cf Hidato problem. CP models:
%   http://www.hakank.org/minizinc/hidato.mzn
%   http://www.hakank.org/minizinc/hidato_exists.mzn
%   http://www.hakank.org/minizinc/hidato_table.mzn
%   http://www.hakank.org/minizinc/hidato_table2.mzn
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: rows = 9;
int: cols = 9;
int: vals = rows*cols;

array[1..rows,1..cols] of int: givens;

% the neighbors for each cell
array[1..rows,1..cols,1..rows,1..cols] of int: neighbors = 
       array4d(1..rows,1..cols,1..rows,1..cols,
                    [ 
                    if abs(i1 - i2) + abs(j1 -j2) == 1 then
                       1
                    else
                       0 
                    endif| i1 in 1..rows, j1 in 1..cols, i2 in 1..rows, j2 in 1..cols
                   ]
               );


% decision variables

%  x[i,j,k] = 1 means cell [i,j] is assigned number k
% var x{i in I, j in J, k in VALS}, binary;
array[1..rows,1..cols,1..vals] of var 0..1: x;


% solve satisfy;
solve :: int_search([x[i,j,k] | i in 1..rows,j in 1..cols,k in 1..vals ], first_fail, indomain_min, complete) satisfy;

constraint
  
  % assign pre-defined numbers using the "givens"
  forall(i in 1..rows, j in 1..cols, k in 1..vals where givens[i,j] != 0) (
     x[i,j,k] = (if givens[i,j] = k then 1 else 0 endif)
  )
  /\

  % each cell must be assigned exactly one number 
  forall(i in 1..rows, j in 1..cols) ( 
     sum(k in 1..vals) (x[i,j,k]) = 1
  )
  /\

  % a value can only occur once 
  forall(k in 1..vals) (
     sum(i in 1..rows, j in 1..cols) (x[i,j,k]) = 1
  )
  /\
  % each cell must have a neighbor with the next higher value 
  forall(i in 1..rows, j in 1..cols, k in 1..vals-1) (
        x[i,j,k] <= sum(i2 in 1..rows, j2 in 1..cols) (x[i2,j2,k+1] * neighbors[i,j,i2,j2])
  )

;

output [
 % show([i1,j1,i2,j2,neighbours[i1,j1,i2,j2]]) ++ "\n"
 % | i1 in 1..rows, j1 in 1..cols, i2 in 1..rows, j2 in 1..cols
 if j = 1 /\ k = 1 then "\n" else "" endif ++
   if fix(x[i,j,k]) = 1 then
      show_int(2, k) ++ " "
   else 
       ""
   endif
 | i in 1..rows, j in 1..cols, k in 1..vals
];

givens = array2d(1..rows,1..cols,
[
  0,  0,  0,  0,  0,  0,  0,  0,  0, 
  0, 11, 12, 15, 18, 21, 62, 61,  0, 
  0,  6,  0,  0,  0,  0,  0, 60,  0, 
  0, 33,  0,  0,  0,  0,  0, 57,  0, 
  0, 32,  0,  0,  0,  0,  0, 56,  0, 
  0, 37,  0,  0,  0,  0,  0, 73,  0, 
  0, 38,  0,  0,  0,  0,  0, 72,  0, 
  0, 43, 44, 47, 48, 51, 76, 77,  0, 
  0,  0,  0,  0,  0,  0,  0,  0,  0, 
]);
