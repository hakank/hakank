% 
% Multi Sudoku in MiniZinc.
% 
% http://stackoverflow.com/questions/34246972/multi-sudoku-ai-approach
% """
% I'm conceptualizing a solver for a variant of sudoku called multi-sudoku, where multiple boards 
% overlap like so:
%
% [image of a multi-sudoku]
%
% If I understand the game correctly, you must solve each grid in such a way that the overlap between 
% any two or more grids is part of the solution for each.
% 
% I'm unsure as to how I should be thinking about this. Anybody got any hints/conceptual clues? 
% Additionally, if any topics in artificial intelligence come to mind, I'd like to hear those too.
% """

% This is a slightly different version of sudoku_multi.mzn
% where the overlapping is in the data section.

% Note: There are 60 solutions.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 9;
int: reg = ceil(sqrt(n));

% decision variables
array[1..n,1..n] of var 1..9: s1;
array[1..n,1..n] of var 1..9: s2;

%
% Solve a Sudoku problem
%
predicate sudoku(array[int,int] of var int: x, int: n, int: reg) =
  forall(i in 1..n) (
    alldifferent([x[i,j] | j in 1..n]) /\
    alldifferent([x[j,i] | j in 1..n])
  )
  /\
  forall(i in 0..reg-1,j in 0..reg-1) (
    alldifferent([x[r,c] | r in i*reg+1..i*reg+reg, c in j*reg+1..j*reg+reg])
  )
;

%
% Print a matrix
%
function array[int] of string: print_matrix(string: s, array[int,int] of var int: x) =
 [show(s) ++ ":"] ++ [if j = 1 then "\n" else " " endif ++ show(s1[i,j]) | i,j in index_set_1of2(x)] ++ ["\n\n"]
;

solve satisfy;
% solve :: int_search(array1d(s1) ++ array1d(s2), first_fail, indomain_split, complete) satisfy;

constraint
  sudoku(s1,n,reg) /\
  sudoku(s2,n,reg)
;

output 
print_matrix("s1", s1)
++
print_matrix("s2", s2)
;

%
% data
%
s1 = array2d(1..n,1..n,
[
_, _, _, _, 6, _, _, _, _,
_, _, _, 4, _, 9, _, _, _,
_, _, 9, 7, _, 5, 1, _, _,
_, 5, 2, _, 7, _, 8, 9, _,
9, _, _, 5, _, 2, _, _, 4,
_, 8, 3, _, 4, _, 7, 2, _,
_, _, _, 2, _, 8, _, _, _,
_, _, _, 6, _, 4, _, _, _,
_, _, _, _, 5, _, _, _, _
]);

% connect s1 and s2 directly
s2 = array2d(1..n,1..n,
[
s1[7,7], s1[7,8], s1[7,9], _, 3, _, _, _, _,
s1[8,7], s1[8,8], s1[8,9], 8, _, 7, _, _, _,
s1[9,7], s1[9,8], s1[9,9], 1, _, 6, 3, _, _,
      _,       9,       8, _, _, _, 1, 2, _,
      2,       _,       _, _, _, _, _, _, 3,
      _,       4,       3, _, _, _, 6, 5, _,
      _,       _,       7, 3, _, 5, 9, _, _,
      _,       _,       _, 4, _, 2, _, _, _,
      _,       _,       _, _, 6, _, _, _, _
]);
