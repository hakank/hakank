% 
% Unhelpful square (Enigma puzzle 1266) in MiniZinc.
% 
% https://enigmaticcode.wordpress.com/2015/01/03/enigma-1266-unhelpful-squares/
% """
% Here is a "cross-figure" for you to do, in which each answer is different:
%   1 2 X 3 _ 
%   X 4 _ _ X
%   5 _ X 6 _
% 
% Across:
%  1. A perfect square
%  2. A perfect square
%  3. A perfect square
%  4. A perfect square
%  5. A perfect square
%  6. A perfect square
%
% Down:
%  2. A perfect square
%  3. A perfect square
% 
% The trouble with cross-figures like this is that there are already digits 
% written in some of the boxes, and that can be confusing. But today whenever 
% you put a digit in such a box it should be different from the digit 
% that's already there. In fact, in half the cases it should be more than the 
% digit already there, and in half the cases it should be less.
% """

%
% This MiniZinc model also show the same (unique) solution as the two
% Python solutions in the comments.
%
%    2 5 X 1 6
%    X 7 8 4 X
%    3 6 X 4 9
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

% data: see below
int: n;
int: m;
array[int] of set of int: boxes;
int: num_boxes = length(boxes);
set of int: blanks;
array[int] of int: clue_numbers;
set of int: perfect_squares = {i*i | i in 4..31 where i*i < 999};

% decision variables
array[1..n,1..m] of var 0..9: x; % matrix
array[1..n*m] of var 0..9: y; % the digits
array[1..num_boxes] of var perfect_squares: nums; % the numbers


% convert an array of digits (a) to a number (which is returned)
function var int: to_num(array[int] of var int: a) =
          let { int: len = card(index_set(a));
                var int: n = sum(i in index_set(a)) (
                   pow(10, len-i) * a[i] 
                 );
         } in n
;

solve satisfy;
% solve :: int_search(y, first_fail, indomain_min, complete) satisfy;

constraint

  % connect "x" (the matrix) and "y" (the digits)
  forall(i in 1..n, j in 1..m) (
    y[(i-1)*m+j] = x[i,j]
  )
  /\ % the numbers (answers)
  forall(b in index_set(boxes)) (
     nums[b] = to_num([y[d] | d in boxes[b]]) /\
     nums[b] in perfect_squares
  )
  /\ all_different(nums) % all answers should be distinct

  /\ % set 0 for the blank cells
  forall(b in blanks) ( y[b] = 0 )

  /\ % % exactly 3 digits should be less and 3 greater than the clue number
  forall(c in index_set(clue_numbers)) (
    (y[clue_numbers[c]] != c) 
  ) 
  /\ sum([y[clue_numbers[c]] < c | c in index_set(clue_numbers)]) = 3
;

output 
[ "x:" ] ++
[ if j = 1 then "\n" else " " endif ++
    if (i-1)*m+j in blanks then
      "X"
    else 
      show(x[i,j])
    endif
  | i in 1..n, j in 1..m
]
;

%
% data
% 

%
% Representation:
%   1  2  3  4  5
%   6  7  8  9 10
%  11 12 13 14 15
%
n = 3;
m = 5;

% the boxes for each number
boxes = 
[
  {1,2},    % 1 across
  {4,5},    % 3 across
  {7,8,9},  % 4 across
  {11,12},  % 5 across
  {14,15},  % 6 across

  {2,7,12}, % 2 down
  {4,9,14}, % 3 down
];

% position of the blanks
blanks = {3,6,10,13};

% position of the digits (clue numbers)
clue_numbers = [1,2,4,7,11,14];
