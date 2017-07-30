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
%
% This version use a table() constraint for the legal moves 
% on a non-torus Moore grid.
%
% Note: this model also solve non-rectangular problems

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

% data
int: r;
int: c;
int: mmax; % end value (!= r*c for non-rectangular problems)
array[1..r, 1..c] of -1..mmax: puzzle;

% It's a waste of time to use the domain -1..max for x if 
% it's a rectangular problem.
int: mmin = if r*c = mmax then 1 else -1 endif;

% decision variables
array[1..r, 1..c] of var mmin..mmax: x;

% solve satisfy;
solve :: int_search(
         [x[i,j] | i in 1..r, j in 1..c], 
         if mmax = r*c then max_regret else most_constrained endif, 
         if mmax = r*c then indomain_min else indomain_median endif,
         complete) satisfy;

% The number of valid connections (on a non-torus More grid) are:
%  4*3                         +   %  the 4 corners has 3 connections each
%  5*(2*(rows-2) + 2*(cols-2)  +   %  the 4 borders has 5 connections
%  8*(rows-2)*(cols-2)         +   %  all the rest has 8 connections
% int: num_connections = 4*3 + 5*(2*(r-2) + 2*(c-2)) + 8*(r-2)*(c-2);
int: num_connections = sum([1 | i1 in 1..r, j1 in 1..c, i2 in 1..r, j2 in 1..c
       where (   abs(i1-i2) <= 1 /\ abs(j1-j2) <= 1 /\ not(i1=i2 /\j1=j2)
                 /\ puzzle[i1,j1] >= 0 /\ puzzle[i2,j2] >= 0
                 )]);

constraint
  trace("num_connections: " ++ show(num_connections) ++ "\n", 1=1)
;

%
% Get all the possible connections for table().
% Here we exposes all four coordinates (i1,j1) (i2,j2)
%
array[1..num_connections, 1..4] of int: valid_connections2 = 
   array2d(1..num_connections, 1..4,
   [ 
     if     k = 1 then i1
     elseif k = 2 then j1
     elseif k = 3 then i2
     else              j2
     endif 
     | i1 in 1..r, j1 in 1..c, i2 in 1..r, j2 in 1..c, k in 1..4
       where ( abs(i1-i2) <= 1 /\ abs(j1-j2) <= 1 /\ not(i1=i2 /\j1=j2) 
               /\ puzzle[i1,j1] >= 0 /\ puzzle[i2,j2] >= 0
              )
   ])
;

constraint

  % all_different_except -1
  all_different([x[i,j] | i in 1..r, j in 1..c where puzzle[i,j] >= 0])

  /\ % place the fixed tiles
  forall(i in 1..r, j in 1..c) (
     if puzzle[i,j] > 0 then
        x[i,j] = puzzle[i,j]
     elseif puzzle[i,j] = -1 then
        x[i,j] = -1
     else
        true
     endif
  )
;

%
% use table constraint
%
constraint

  forall(s in 1..mmax-1) (
     let {
        var 1..r: i1, var 1..c: j1,
        var 1..r: i2, var 1..c: j2
     }
     in 
       % get where "this" number is and ensure that
       % the next is around
       x[i1,j1] = s /\ x[i2,j2] = s+1
       /\
       table([i1,j1,i2,j2], valid_connections2) % :: domain 
       /\
       if mmax != r*c then
         puzzle[i1,j1] >= 0
         /\
         puzzle[i2,j2] >= 0
       else
         true
       endif
  )

;


output 
[
  if j = 1 then "\n" else " " endif ++
    if fix(x[i,j]) < 0 then " " else if fix(x[i,j]) < 10 then "  " else if fix(x[i,j]) < 100 then " " else "" endif endif endif ++
      if fix(x[i,j]) > 0 then 
         show(x[i,j])
      else 
         "__"
      endif
  | i in 1..r, j in 1..c

] ++ ["\n"];

%
% data
%
% 0 are the unknowns.
% For non-rectangular problems, -1 is non-squares.

% solution:
%  6 7 9
%  5 2 8
%  1 4 3
% r=3;
% c=3;
% mmax = r*c;
% puzzle = array2d(1..r, 1..c,
%    [ 
%       6,0,9,
%       0,2,8,
%       1,0,0
%    ]);

% some tests
% r=3;
% c=3;
% mmax = r*c;
% puzzle = array2d(1..r, 1..c,
%     [ 
%        1,0,0,
%        0,0,0,
%        0,0,0
%    ]);

% r=4;
% c=4;
% mmax = r*c;
% puzzle = array2d(1..r, 1..c,
%     [ 
%        1,0,0,0,
%        0,2,0,0,
%        0,0,3,0,
%        0,0,0,4,
%    ]);




% solution 
% 
% 45 44 41 40 39 30 31
% 46 43 42 28 29 38 32
% 47 1 3 26 27 33 37
% 48 2 25 4 34 35 36
% 49 16 24 23 5 6 8
% 17 19 15 22 12 7 9
% 18 20 21 14 13 11 10
% r = 7;
% c = 7;
% mmax = r*c;
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
% c=5;
% mmax = r*c;
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
% c=5;
% mmax = r*c;
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
% c=6;
% mmax = r*c;
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
% c=8;
% mmax = r*c;
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
% c = 10;
% mmax = r*c;
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


% Problem 188 (Genius)
% r = 12;
% c = 12;
% mmax = r*c;
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


% Non-rectangular problem from
% http://en.wikipedia.org/wiki/Hidato
% -1 is non-accessable cells.
r = 8;
c = 8;
mmax = 40;
puzzle = array2d(1..r, 1..c, 
[
  0,33,35, 0, 0,-1,-1,-1,
  0, 0,24,22, 0,-1,-1,-1,
  0, 0, 0,21, 0, 0,-1,-1,
  0,26, 0,13,40,11,-1,-1,
 27, 0, 0, 0, 9, 0, 1,-1,
 -1,-1, 0, 0,18, 0, 0,-1,
 -1,-1,-1,-1, 0, 7, 0, 0,
 -1,-1,-1,-1,-1,-1, 5, 0
]);

