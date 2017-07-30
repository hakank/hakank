% 
% Hidato puzzle in MiniZinc.
% 
% http://www.shockwave.com/gamelanding/hidato.jsp
% http://www.hidato.com/
%
% """
% Puzzles start semi-filled with numbered tiles.
% The first and last numbers are circled.
% Connect the numbers together to win. Consecutive
% number must touch horizontally, vertically, or
% diagonally.
% """

% Some statistics:
% For a 3 x 3 puzzle the following number of puzzles are possible,
% given the position of 1:
% 
%   1 0 0
%   0 0 0 
%   0 0 0
% 138 possible solutions
%
%   0 1 0
%   0 0 0
%   0 0 0
% 50 possible solutions
%
%   0 0 0
%   0 1 0
%   0 0 0
% 32 possible solutions

% All solutions:
%   0 0 0
%   0 0 0
%   0 0 0
% 784 possible solutions


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: r;
int: c=r;
array[1..r, 1..c] of int: puzzle;

array[1..r, 1..c] of var 1..r*c: x; %;

% solve satisfy;
solve :: int_search(
            [x[i,j] | i in 1..r, j in 1..c], 
            first_fail, 
            indomain_min, 
            complete) 
        satisfy;

constraint
  % place all integers from 1..r*c
  all_different([x[i,j] | i in 1..r, j in 1..c]) :: domain

  /\ % place the fixed tiles
  forall(i in 1..r, j in 1..c) (
     puzzle[i,j] > 0 -> 
         x[i,j] = puzzle[i,j]
  )

%   /\ % well, this works but is quite inefficient...
%   forall(k in 1..r*c-1) (
%      exists(i in 1..r, j in 1..c) (
%         k = x[i, j] % fix this k
%         /\
%         exists(a, b in  {-1, 0, 1}
%           where 
%           i+a >= 1 /\ j+b >=  1 /\
%           i+a <= r /\ j+b <= c
%           /\ not(a = 0 /\ b = 0) 
%           % /\ abs(a) + abs(b) >= 1 
%         ) 
%         (
%           % find the next k
%           k + 1 = x[i+a, j+b]  
%         )
%      )
%   )


  /\ % This variant of "expanding" the exists is more efficient
  forall(k in 1..r*c-1) (
     let {
        var 1..r: i,
        var 1..c: j,
        var {-1,0,1}: a,
        var {-1,0,1}: b
     }
     in
     k = x[i, j] % fix this k
     /\
     i+a >= 1 /\ j+b >=  1 /\
     i+a <= r /\ j+b <= c
     /\ not(a = 0 /\ b = 0) 
     /\ 
     % find the next k
     k + 1 = x[i+a, j+b]  
  )

;

output 
% [
%  "pos: ", show(pos),
% ] ++
[
  if j = 1 then "\n" else " " endif ++
    show_int(3, x[i,j])
  | i in 1..r, j in 1..c

];

%
% data
%
% 0 are the unknowns

% solution:
%  6 7 9
%  5 2 8
%  1 4 3
r=3;
% c=3;
puzzle = array2d(1..r, 1..c,
   [ 
      6,0,9,
      0,2,8,
      1,0,0
   ]);

% some tests
% r=3;
% puzzle = array2d(1..r, 1..c,
%     [ 
%        1,0,0,
%        0,0,0,
%        0,0,0
%    ]);

% r=4;
% puzzle = array2d(1..r, 1..c,
%     [ 
%        1,0,0,0,
%        0,2,0,0,
%        0,0,3,0,
%        0,0,0,4,
%    ]);




% solution 
% fz: 1.3 seconds (was 4.5 seconds)
% 
% 45 44 41 40 39 30 31
% 46 43 42 28 29 38 32
% 47 1 3 26 27 33 37
% 48 2 25 4 34 35 36
% 49 16 24 23 5 6 8
% 17 19 15 22 12 7 9
% 18 20 21 14 13 11 10
% r = 7;
% puzzle = array2d(1..r, 1..c,
%     [ 
%        0,44,41, 0, 0, 0, 0,
%        0,43, 0,28,29, 0, 0,
%        0, 1, 0, 0, 0,33, 0,
%        0, 2,25, 4,34, 0,36,
%       49,16, 0,23, 0, 0, 0,
%        0,19, 0, 0,12, 7, 0,
%        0, 0, 0,14, 0, 0, 0 
%     ]);
 

% Problem from the book:
% Gyora Bededek: "Hidato: 2000 Pure Logic Puzzles"

% problem 1 (Practice)
% r=5;
% puzzle = array2d(1..r, 1..c,
%   [
%       0, 0,20, 0, 0,
%       0, 0, 0,16,18,
%      22, 0,15, 0, 0,
%      23, 0, 1,14,11,
%       0,25, 0, 0,12,
%   ]);

% % problem 2 (Practice)
% r=5;
% puzzle = array2d(1..r, 1..c,
%   [
%       0, 0, 0, 0,14,
%       0,18,12, 0, 0,
%       0, 0,17, 4, 5,
%       0, 0, 7, 0, 0,
%       9, 8,25, 1, 0,
%   ]);

% problem 3 (Beginner)
% r=6;
% puzzle = array2d(1..r, 1..c,
%    [
%       0, 26,0,0,0,18,
%       0,0,27,0,0,19,
%       31,23,0,0,14,0,
%       0,33,8,0,15,1,
%       0,0,0,5,0,0,
%       35,36,0,10,0,0
%    ]);


% Problem 15 (Intermediate)
% r=8;
% puzzle = array2d(1..r, 1..c,
%    [
%     64, 0, 0, 0, 0, 0, 0, 0,
%      1,63, 0,59,15,57,53, 0,
%      0, 4, 0,14, 0, 0, 0, 0,
%      3, 0,11, 0,20,19, 0,50,
%      0, 0, 0, 0,22, 0,48,40,
%      9, 0, 0,32,23, 0, 0,41,
%     27, 0, 0, 0,36, 0,46, 0,
%     28,30, 0,35, 0, 0, 0, 0,
%    ]);


% Problem 156 (Master)
% (This seems to be harder to solve than the 12x12 prolem 188 below...)
% r = 10;
% puzzle = array2d(1..r, 1..c,
%   [
%    88, 0, 0,100, 0, 0,37,0, 0,34,
%     0,86, 0,96,41, 0, 0,36, 0, 0,
%     0,93,95,83, 0, 0, 0,31,47, 0,
%     0,91, 0, 0, 0, 0, 0,29, 0, 0,
%     11,0, 0, 0, 0, 0, 0,45,51, 0,
%     0, 9, 5, 3, 1, 0, 0, 0, 0, 0,
%     0,13, 4, 0, 0, 0, 0, 0, 0, 0,
%     15,0, 0,25, 0, 0,54,67, 0, 0,
%     0,17, 0,23, 0,60,59, 0,69, 0,
%     19,0,21,62,63, 0, 0, 0, 0, 0,
%   ]);


% % Problem 188 (Genius)
% r = 12;
% puzzle = array2d(1..r, 1..c,
%  [
%      0,  0,134,  2,  4,  0,  0,  0,  0,  0,  0,  0,
%    136,  0,  0,  1,  0,  5,  6, 10,115,106,  0,  0,
%    139,  0,  0,124,  0,122,117,  0,  0,107,  0,  0,
%      0,131,126,  0,123,  0,  0, 12,  0,  0,  0,103,
%      0,  0,144,  0,  0,  0,  0,  0, 14,  0, 99,101,
%      0,  0,129,  0, 23, 21,  0, 16, 65, 97, 96,  0,
%     30, 29, 25,  0,  0, 19,  0,  0,  0, 66, 94,  0,
%     32,  0,  0, 27, 57, 59, 60,  0,  0,  0,  0, 92,
%      0, 40, 42,  0, 56, 58,  0,  0, 72,  0,  0,  0,
%      0, 39,  0,  0,  0,  0, 78, 73, 71, 85, 69,  0,
%     35,  0,  0, 46, 53,  0,  0,  0, 80, 84,  0,  0,
%     36,  0, 45,  0,  0, 52, 51,  0,  0,  0,  0, 88,
%  ]);
