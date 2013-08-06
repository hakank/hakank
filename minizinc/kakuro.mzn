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
% Also, see the following related models:
%  - Comet: http://www.hakank.org/comet/kakuro.co
%  - MiniZinc: http://www.hakank.org/comet/kenken2.mzn
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 7;
array[1..n, 1..n] of var 0..9: x;

%
% state the problem (without the operation)
%
% For a better view of the problem, see
%  http:%en.wikipedia.org/wiki/File:KenKenProblem.svg
%
int: num_p = 24; % number of segments
int: num_hints = 5;  % number of hints per segments (that's max number of hints)
int: max_val = 100;
array[1..num_p, 1..2*num_hints+1] of 0..max_val: P =
  array2d(1..num_p, 1..2*num_hints+1, [
   1,1, 1,2, 0,0, 0,0, 0,0,  16,
   1,5, 1,6, 1,7, 0,0, 0,0,  24,
   2,1, 2,2, 0,0, 0,0, 0,0,  17,
   2,4, 2,5, 2,6, 2,7, 0,0,  29,
   3,1, 3,2, 3,3, 3,4, 3,5,  35,
   4,2, 4,3, 0,0, 0,0, 0,0,   7, 
   4,5, 4,6, 0,0, 0,0, 0,0,   8, 
   5,3, 5,4, 5,5, 5,6, 5,7,  16, 
   6,1, 6,2, 6,3, 6,4, 0,0,  21, 
   6,6, 6,7, 0,0, 0,0, 0,0,   5, 
   7,1, 7,2, 7,3, 0,0, 0,0,   6, 
   7,6, 7,7, 0,0, 0,0, 0,0,   3, 

   1,1, 2,1, 3,1, 0,0, 0,0,  23, 
   1,2, 2,2, 3,2, 4,2, 0,0,  30, 
   1,5, 2,5, 3,5, 4,5, 5,5,  27, 
   1,6, 2,6, 0,0, 0,0, 0,0,  12, 
   1,7, 2,7, 0,0, 0,0, 0,0,  16, 
   2,4, 3,4, 0,0, 0,0, 0,0,  17,    
   3,3, 4,3, 5,3, 6,3, 7,3,  15, 
   4,6, 5,6, 6,6, 7,6, 0,0,  12, 
   5,4, 6,4, 0,0, 0,0, 0,0,   7,    
   5,7, 6,7, 7,7, 0,0, 0,0,   7, 
   6,1, 7,1, 0,0, 0,0, 0,0,  11, 
   6,2, 7,2, 0,0, 0,0, 0,0,  10
   ]);


%
% the blanks, coded as 0 in x
%
int: num_blanks = 13;
int: max_blanks_cells = 3;
array[1..2*num_blanks] of 1..n: blanks = 
  [
   1,3, 1,4,
   2,3,
   3,6, 3,7,
   4,1, 4,4, 4,7,
   5,1, 5,2,
   6,5,
   7,4, 7,5
   ];


solve :: int_search([x[i,j] | i,j in 1..n], first_fail, indomain_max, complete) satisfy;

constraint
 
  forall(i in 1..num_blanks) (
        x[ blanks[2*(i-1)+1], blanks[2*(i-1)+2]] = 0
  )
  /\
  forall(p in 1..num_p) (
    (sum(i in 1..num_hints where P[p,2*(i-1)+1] > 0) (x[  P[p, 2*(i-1)+1], P[p,2*(i-1)+2]  ]) = P[p, 2*num_hints+1])
    /\
    all_different(i in 1..num_hints where P[p,2*(i-1)+1] > 0) (
        x[P[p, 2*(i-1)+1], P[p, 2*(i-1)+2]]
    )
   /\
   forall(i in 1..num_hints where P[p,2*(i-1)+1] > 0) (
        x[P[p, 2*(i-1)+1], P[p, 2*(i-1)+2]] > 0
   )
  )
;

output [
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
];
