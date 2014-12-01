% 
% Project Euler problem 18 in MiniZinc.
% 
% http://projecteuler.net/index.php?section=problems&id=18
% """
% By starting at the top of the triangle below and moving to adjacent numbers 
% on the row below, the maximum total from top to bottom is 23.
%
%    3
%   7 5
%  2 4 6
% 8 5 9 3
%
% That is, 3 + 7 + 4 + 9 = 23.
%
% Find the maximum total from top to bottom of the triangle below:
%
%                   75
%                  95 64
%                17 47 82
%               18 35 87 10
%              20 04 82 47 65
%            19 01 23 75 03 34
%           88 02 77 73 07 63 67
%          99 65 04 28 06 16 70 92
%        41 41 26 56 83 40 80 70 33
%       41 48 72 33 47 32 37 16 94 29
%     53 71 44 65 25 43 91 52 97 51 14
%    70 11 33 28 77 73 17 78 39 68 17 57
%   91 71 52 38 17 14 91 43 58 50 27 29 48
%  63 66 04 68 89 53 67 30 73 16 69 87 40 31
% 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
%
% NOTE: As there are only 16384 routes, it is possible to solve this problem by
% trying every route. However, Problem 67, is the same challenge with a 
% triangle containing one-hundred rows; it cannot be solved by brute force, 
% and requires a clever method! ;o)
% """

% Solution: 1074

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n;
% array[1..n,1..n+(n-1)] of 0..9: triangle;
array[1..n,1..n] of int: triangle;
% array[1..n] of var 1..n+(n-1): x;
array[1..n] of var 1..n: x;
var int: total;


% solve maximize total;
solve :: int_search(x, max_regret, indomain_split, complete) maximize total;

constraint
    forall(i in 2..n) (
       triangle[i,x[i]] > 0
       /\
       triangle[i-1,x[i-1]] > 0
       /\
       %abs(x[i]-x[i-1]) = 1
       % (x[i]-x[i-1] = 1 \/ x[i]-x[i-1] = 0)
       x[i]-x[i-1]>=0 /\ x[i]-x[i-1]<=1
    )
    /\
    x[1] = 1
    /\
    total = sum(i in 1..n) (triangle[i, x[i]])

;


% the simple example
% n = 4;
% triangle = array2d(1..n, 1..n, 
%   [
%    3,0,0,0,
%    7,5,0,0,
%    2,4,6,0,
%    8,5,9,3
% ]);


% the real problem
n = 15;
triangle = array2d(1..n, 1..n, [
75, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
95,64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
17,47,82, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
18,35,87,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
20,04,82,47,65, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
19,01,23,75,03,34, 0, 0, 0, 0, 0, 0, 0, 0, 0,
88,02,77,73,07,63,67, 0, 0, 0, 0, 0, 0, 0, 0,
99,65,04,28,06,16,70,92, 0, 0, 0, 0, 0, 0, 0,
41,41,26,56,83,40,80,70,33, 0, 0, 0, 0, 0, 0,
41,48,72,33,47,32,37,16,94,29, 0, 0, 0, 0, 0,
53,71,44,65,25,43,91,52,97,51,14, 0, 0, 0, 0,
70,11,33,28,77,73,17,78,39,68,17,57, 0, 0, 0,
91,71,52,38,17,14,91,43,58,50,27,29,48, 0, 0,
63,66,04,68,89,53,67,30,73,16,69,87,40,31, 0,
04,62,98,27,23,09,70,98,73,93,38,53,60,04,23,
]);


output [
  "\ntotal: ", show(total), "\n",
  "x: ", show(x),"\n"
] ++ [
  
  show(triangle[i, x[i]])++ " "
  | i in 1..n
];


