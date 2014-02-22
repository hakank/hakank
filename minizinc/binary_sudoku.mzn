% 
% Binary Sudoku in MiniZinc.
% 
% From http://eclipseclp.org/examples/binsudoku.ecl.txt
% The binary sudoku as explained here
% http://cstheory.stackexchange.com/questions/16982/how-hard-is-binary-sudoku-puzzle

% An NxN matrix must be filled with 0s and 1s such that:
%  - each row and each column contains an equal number of 0s and 1s
%  - no two rows or two columns are identical
%  - no sequences of 3 or more consecutive 0s or 1s in rows or columns
%
% ?- solve(2, M).
% M = []([](1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0),
%        [](0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1),
%        [](1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0),
%        [](1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0),
%        [](0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1),
%        [](0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0),
%        [](1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1),
%        [](1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1),
%        [](0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0),
%        [](0, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0),
%        [](1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1),
%        [](0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1))
% Yes (0.03s cpu, solution 1, maybe more)
% """
%
% Also see 
% * Marzio De Biasi: "Binary Puzzle is NP-complete"
%   http://www.fractalmuse.org/vdisk/cstheory/binaryp.pdf


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: X = -1;
int: n;
int: k = n div 2;
array[1..n,1..n] of int: puzzle;

% decision variables
array[1..n,1..n] of var 0..1: x;


predicate lex_ne(array[int] of var int: a1,
                 array[int] of var int: a2
                 ) = 
   sum([bool2int(a1[i] != a2[i]) | i in 1..length(a1)]) > 0
;

% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..n], first_fail, indomain_min, complete) satisfy;

constraint
  forall(i,j in 1..n) (
    if puzzle[i,j] >= 0 then
      x[i,j] = puzzle[i,j]
    else
      true
    endif
  )

  /\
  forall(i in 1..n) (
    sum([x[i,j] | j in 1..n]) = k /\
    sum([x[j,i] | j in 1..n]) = k /\

    sliding_sum (1, 2, 3, [x[i,j] | j in 1..n ]) /\
    sliding_sum (1, 2, 3, [x[j,i] | j in 1..n ]) /\


    forall(j in i+1..n) (
      lex_ne([x[i,c] | c in 1..n], [x[j,c] | c in 1..n]) /\
      lex_ne([x[c,i] | c in 1..n], [x[c,j] | c in 1..n])

    )
    
  )
;

output [
  if j=1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
];

n = 12;
puzzle = array2d(1..n, 1..n,
[
 X,1,0,X,X,X,X,0,X,X,0,X,
 X,1,1,X,X,1,X,X,X,X,X,X,
 X,X,X,X,X,X,X,X,1,X,X,0,
 X,X,0,0,X,X,X,X,X,X,X,0,
 X,X,X,X,X,X,1,1,X,0,X,X,
 X,1,X,0,X,1,1,X,X,X,1,X,
 X,X,X,X,X,X,X,X,1,X,X,X,
 1,X,X,1,X,X,X,X,X,X,0,X,
 X,1,X,X,X,X,X,X,0,X,X,X,
 X,X,X,X,X,X,X,0,X,X,X,X,
 1,X,X,X,X,X,X,X,X,X,X,1,
 X,1,X,1,X,X,X,X,X,0,0,X,
]);