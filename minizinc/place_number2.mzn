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

int: n;
array[1..n] of var 1..n: x;

% the problem
array[1..n] of set of 1..n: g;

solve satisfy;
% solve :: int_search(x, input_order, indomain_min, complete) satisfy;

constraint
  all_different(x)
  /\
  forall(i in 1..n) (
     forall(j in g[i]) (
        abs(x[i]-x[j]) > 1 
     )
  )
  %% symmetry breaking
  /\ x[1] < x[n]
  /\ x[1] < x[2]
;

% Problem 1
% output [
%    show(x) ++ "\n\n" ++ 
%    "  " ++ show(x[2]) ++ " " ++ show(x[5]) ++ "\n" ++
%    show(x[1]) ++ " " ++ show(x[3]) ++ " " ++ show(x[6]) ++ " " ++ show(x[8]) ++ "\n" ++
%    "  " ++ show(x[4]) ++ " " ++ show(x[7]) ++ "\n"  
% ];

% Problem 2
output [
    show(x) ++ "\n\n" ++ 

    "   " ++ show_int(2, x[3]) ++ " " ++ show_int(2, x[7]) ++ "\n" ++
    show_int(2, x[1]) ++ " " ++ show_int(2, x[4]) ++ " " ++ show_int(2, x[8]) ++ " " ++ show_int(2, x[11]) ++ "\n" ++
    show_int(2, x[2]) ++ " " ++ show_int(2, x[5]) ++ " " ++ show_int(2, x[9]) ++ " " ++ show_int(2, x[12]) ++ "\n" ++
    "   " ++ show_int(2, x[6]) ++ " " ++ show_int(2, x[10])
];


% Problem 1: 8 sized
% 
% (representation)
%   2 5
% 1 3 6 8
%   4 7
% 
% n = 8;
% g = [
%   {2,3,4},       % 1
%   {1,3,5,6},     % 2
%   {2,4,6,7},     % 3
%   {1,3,6,7},     % 4
%   {2,3,6,8},     % 5
%   {2,3,4,5,7,8}, % 6
%   {3,4,6,8},     % 7
%   {5,6,7}        % 8 
%   ];


%
% Problem 2: 12 sized
% (representation)
%   3 7
% 1 4 8 11
% 2 5 9 12
%   6 10
%
n = 12;
g = [
  {2,3,4,5}, % 1
  {1,4,5,6}, % 2
  {1,4,7,8}, % 3
  {1,2,3,5,7,8,9}, % 4
  {1,2,4,6,8,9,10}, % 5      
  {2,5,9,10}, % 6
  {3,4,8,11}, % 7
  {3,4,5,7,9,11,12}, % 8
  {4,5,6,8,10,11,12}, % 9
  {5,6,9,12}, % 10
  {7,8,9,12}, % 11
  {8,9,10,11}, % 12
];
