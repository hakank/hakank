% 
% Ski assignment problem in MiniZinc.
%
% From
% Jeffrey Lee Hellrung, Jr.: PIC 60, Fall 2008 – Final Review, December 12, 2008
% http://www.math.ucla.edu/~jhellrun/course_files/Fall%25202008/PIC%252060%2520-%2520Data%2520Structures%2520and%2520Algorithms/final_review.pdf
% """
% 5. Ski Optimization! Your job at Snapple is pleasant but in the winter you've 
% decided to become a ski bum. You've hooked up with the Mount Baldy Ski Resort. 
% They'll let you ski all winter for free in exchange for helping their ski rental 
% shop with an algorithm to assign skis to skiers. Ideally, each skier should 
% obtain a pair of skis whose height matches his or her own height exactly. 
% Unfortunately, this is generally not possible. We define the disparity between 
% a skier and his or her skis to be the absolute value of the difference between 
% the height of the skier and the pair of skis. Our objective is to find an 
% assignment of skis to skiers that minimizes the sum of the disparities. 
% ...
% Illustrate your algorithm by explicitly filling out the A[i, j] table for the 
% following sample data:
%   * Ski heights: 1, 2, 5, 7, 13, 21.
%   * Skier heights: 3, 4, 7, 11, 18.
%
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
% include "globals-gecode.mzn"; 

int: num_skis = 6;
int: num_skiers = 5;
array[1..num_skis] of int: ski_heights;
array[1..num_skiers] of int: skier_heights;

% the assignments
array[1..num_skiers] of var 1..num_skis: x;

% minimize the differences
% var int: z = sum(i in 1..num_skiers) ( abs(ski_heights[x[i]] - skier_heights[i]) );
% Without the element constraint
var int: z = sum(i in 1..num_skiers, j in 1..num_skis) (
             bool2int(x[i] = j) * abs(ski_heights[j] - skier_heights[i]) 
    );

% solve satisfy;
solve :: int_search(x, smallest, indomain_min, complete)  minimize z;

constraint
   % z = 7 /\ % for original problem
   all_different(x) % :: domain
;

ski_heights =   [1, 2, 5, 7, 13, 21]; % original problem
skier_heights = [3, 4, 7, 11, 18];

output [
  show(z) ++ "\n" ++
  show(x) ++ "\n"
];