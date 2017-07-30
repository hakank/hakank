% 
% Place number puzzle in MiniZinc.
% 
% From
% http://codegolf.stackexchange.com/questions/8571/a-programming-challenge-number-cross-puzzle
% """
% This is from a programming challenge website.
% 
% The cross problem states that you can place the numbers 1 through N inside the cross such 
% that no two adjacent cells in any direction (up, down, left or right, or diagonal) contain 
% adjacent numbers (+ or - 1).
% 
% Given the shape of an 8 sized cross below where the numbers represent the cell index:
% 
%   1 4
% 0 2 5 7
%   3 6
% 
% there are only 4 solutions for an 8 sized cross. In the above structure, cell 2 is adjacent to 
% cells 0, 1, 3, 4, 5 and 6 while cell 1 is adjacent to cell 0, 2, 4 and 5. Here is one of 
% the 4 solutions:
%
%   5 3
% 2 8 1 7
%   6 4
% 
% How many solutions are there on a 12 sized cross given the indexes below?
% 
%   2 6
% 0 3 7 10
% 1 4 8 11
%   5 9
% 
% I solved it via simple brute force where I used recursion to fill the cross. It took me 
% around 5-10 mins though. I wonder whether there is an efficient algorithm for it, or maybe 
% a mathematical way. I really liked this challenge and that is why I am wondering about 
% an efficient solution.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: rows;
int: cols;
int: n = sum(i in 1..rows, j in 1..cols) ( bool2int(mat[i,j] > 0) );

% the problem grid
array[1..rows, 1..cols] of int: mat;

% decision variables
array[1..n] of var 1..n: x;

solve satisfy;
% solve :: int_search(x, input_order, indomain_min, complete) satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(x) % :: domain
  /\
  forall(i in 1..rows, j in 1..cols where mat[i,j] > 0) (
     forall(a,b in {-1,0,1} where 
            i+a >= 1  /\ j+b >=  1 /\
            i+a <= rows /\ j+b <= cols /\
            not(a = 0 /\ b = 0) /\
            mat[i+a,j+b] > 0
         )
           ( abs(x[mat[i,j]]- x[mat[i+a,j+b]])>1 )
   )

  %% symmetry breaking
  % /\ x[1] < x[n]
  % /\ x[1] < x[2]
;

output
[
  show(x)
];



%
% Problem 0: 4 sized. 
% Don't have a solution.
% 
% rows = 3;
% cols = 3;

% % 0 means empty cell
% mat = array2d(1..rows, 1..cols,
% [
%  0, 2, 0,
%  1, 3, 5,
%  0, 4, 0
% ]);



% Problem 1: 8 sized
% 
%   2 5
% 1 3 6 8
%   4 7
% 
% rows = 3;
% cols = 4;

% % 0 means empty cell
% mat = array2d(1..rows, 1..cols,
% [
%  0, 2, 5, 0,
%  1, 3, 6, 8,
%  0, 4, 7, 0
% ]);


%
% Problem 2: 12 sized
% 
%   3 7
% 1 4 8 11
% 2 5 9 12
%   6 10
%
rows = 4;
cols = 4;

% 0 means empty cell
mat = array2d(1..rows, 1..cols,
[
 0, 3, 7, 0,
 1, 4, 8, 11,
 2, 5, 9, 12,
 0, 6, 10, 0
]);


%
% Another problem 
%
% rows = 5;
% cols = 5;

% % 0 means empty cell
% mat = array2d(1..rows, 1..cols,
% [
%  0, 4, 9,14, 0,
%  1, 5,10,15,19,
%  2, 6,11,16,20,
%  3, 7,12,17,21,
%  0, 8,13,18, 0,
% ]);

% A variant
% rows = 5;
% cols = 5;

% % 0 means empty cell
% mat = array2d(1..rows, 1..cols,
% [
%  0, 0, 5, 0, 0,
%  0, 2, 6,10, 0,
%  1, 3, 7,11,13,
%  0, 4, 8,12, 0,
%  0, 0, 9, 0, 0,
% ]);
