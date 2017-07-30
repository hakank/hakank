% 
% Survo puzzle in MiniZinc.
% 
% http://en.wikipedia.org/wiki/Survo_Puzzle
% """
% Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied 
% by Seppo Mustonen. The name of the puzzle is associated to Mustonen's 
% Survo system which is a general environment for statistical computing and 
% related areas.
% 
% In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n so 
% that each of these numbers appears only once and their row and column sums are 
% equal to integers given on the bottom and the right side of the table. 
% Often some of the integers are given readily in the table in order to 
% guarantee uniqueness of the solution and/or for making the task easier.
% """
% 
% See also
% http://www.survo.fi/english/index.html
% http://www.survo.fi/puzzles/index.html
%
% References:
% Mustonen, S. (2006b). "On certain cross sum puzzles"
% http://www.survo.fi/papers/puzzles.pdf 
% Mustonen, S. (2007b). "Enumeration of uniquely solvable open Survo puzzles." 
% http://www.survo.fi/papers/enum_survo_puzzles.pdf 
% Kimmo Vehkalahti: "Some comments on magic squares and Survo puzzles" 
% http://www.helsinki.fi/~kvehkala/Kimmo_Vehkalahti_Windsor.pdf
% R code: http://koti.mbnet.fi/tuimala/tiedostot/survo.R

%
% Compare with
%  * magic_square*.mzn, which has the same row/sum sum and diagonal constraints
%    magic squares can be seen as a special case of survo puzzles.  
%  * puzzle1.mzn, which also requires constraints on diagonals, but 
%    no there is no all_different constraint.
%  * tomography*.mzn: which uses binary values in the matrix.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: r; % number of rows 
int: c; % number of columns
array[1..r] of int: rowsums;
array[1..c] of int: colsums;
array[1..r, 1..c] of 0..r*c: matrix;  % known values (the clues)

% the solution:
array[1..r, 1..c] of var 1..r*c: x;

% solve satisfy;
solve :: int_search([x[i,j] | i in 1..r, j in 1..c], 
        first_fail, indomain_min, complete) satisfy;

constraint
  all_different([x[i,j] | i in 1..r, j in 1..c])
  /\

  forall(i in 1..r, j in 1..c where matrix[i,j] > 0) (
     matrix[i,j] = x[i,j]
  )
  /\
  forall(i in 1..r) (
     sum(j in 1..c) (x[i,j]) = rowsums[i]
  )
  /\
  forall(j in 1..c) (
   sum(i in 1..r) (x[i,j]) = colsums[j]
  )
;

output [
  "["
]
++
[
  if j = 1 then "\n" else "," endif ++
    if fix(x[i,j]) < 10 then "  " else " "  endif ++
    show(x[i,j]) ++
  if j = c then 
    ", f(" ++ show(rowsums[i]) ++ "),"
  else 
    ""
  endif
  | i in 1..r, j in 1..c
] 
++
["\n"]++
[
  "f(" ++ show(colsums[j]) ++ "),"
  | j in 1..c
]

++ ["]\n"]
;


%
% data
%

% http://en.wikipedia.org/wiki/Survo_Puzzle, first example
% rowsums = [30,18,30];
% colsums = [27,16,10,25];
% matrix = array2d(1..r, 1..c, 
%   [
%     0, 6, 0, 0,
%     8, 0, 0, 0,
%     0, 0, 3, 0 
%   ]);
% r = 3;
% c = 4;


% http://en.wikipedia.org/wiki/Survo_Puzzle, second example
% difficulty 0
% r = 2;
% c = 3;
% rowsums = [9,12];
% colsums = [9, 7, 5];
% matrix = array2d(1..r, 1..c, 
%    [
%      0, 0, 3,
%      0, 6, 0
%    ]);

% http://en.wikipedia.org/wiki/Survo_Puzzle, third example
% difficulty 150 ("open puzzle", i.e. no hints)
% It's an unique solution.
% (817 propagations with Gecode/fz, and 33 failures, 88 commits)
% r = 3;
% c = 4;
% rowsums = [24,15,39];
% colsums = [21,10,18,29];
% matrix = array2d(1..r, 1..c, 
%   [
%     0, 0, 0, 0,
%     0, 0, 0, 0,
%     0, 0, 0, 0
%   ]);

% same as above but with hints: difficulty 0
% (15 propagations with Gecode/fz, no failures, no commits)
% matrix = array2d(1..r, 1..c, 
%    [
%      7, 0, 5, 0,
%      0, 1, 0, 8,
%      0, 0, 11, 0
%    ]);


% http://en.wikipedia.org/wiki/Survo_Puzzle, under "Swapping method"
% (open puzzle)
% Gecode/fz: 13374 propagations, 706 failures
% r = 4;
% c = 4;
% rowsums = [51,36,32,17];
% colsums = [51,42,26,17];
% matrix = array2d(1..r, 1..c, 
%   [
%     0, 0, 0, 0,
%     0, 0, 0, 0,
%     0, 0, 0, 0,
%     0, 0, 0, 0
%   ]);


% http://www.survo.fi/puzzles/280708.txt, third puzzle
% Survo puzzle 128/2008 (1700) #364-35846
%
%    A  B  C  D  E  F
% 1  *  *  *  *  *  * 30
% 2  *  * 18  *  *  * 86
% 3  *  *  *  *  *  * 55
%   22 11 42 32 27 37
% r = 3;
% c = 6;
% rowsums = [30, 86, 55];
% colsums = [22, 11, 42, 32, 27, 37];
% matrix = array2d(1..r, 1..c, 
%   [
%     0, 0,  0, 0, 0, 0,
%     0, 0, 18, 0, 0, 0,
%     0, 0,  0, 0, 0, 0
%   ]);

% Survo puzzle 269/2012 (920) #443-74130
%    A  B  C  D
% 1  *  *  *  * 16
% 2  *  *  *  * 40
% 3  *  *  *  * 53
% 4  *  *  *  * 27
%   16 32 40 48
%
% r = 4;
% c = 4;
% rowsums = [16, 40, 53, 27];
% colsums = [16, 32, 40, 48];
% matrix = array2d(1..r, 1..c, 
%   [
%     0, 0, 0, 0,
%     0, 0, 0, 0,
%     0, 0, 0, 0,
%     0, 0, 0, 0,
%   ]);
