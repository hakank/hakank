/*

  Ski assignment problem in AMPL+CP.

  From
  Jeffrey Lee Hellrung, Jr.: PIC 60, Fall 2008 – Final Review, December 12, 2008
  http://www.math.ucla.edu/~jhellrun/course_files/Fall%25202008/PIC%252060%2520-%2520Data%2520Structures%2520and%2520Algorithms/final_review.pdf
  """
  5. Ski Optimization! Your job at Snapple is pleasant but in the winter you've 
  decided to become a ski bum. You've hooked up with the Mount Baldy Ski Resort. 
  They'll let you ski all winter for free in exchange for helping their ski rental 
  shop with an algorithm to assign skis to skiers. Ideally, each skier should 
  obtain a pair of skis whose height matches his or her own height exactly. 
  Unfortunately, this is generally not possible. We define the disparity between 
  a skier and his or her skis to be the absolute value of the difference between 
  the height of the skier and the pair of skis. Our objective is to find an 
  assignment of skis to skiers that minimizes the sum of the disparities. 
  ...
  Illustrate your algorithm by explicitly filling out the A[i, j] table for the 
  following sample data:
    * Ski heights: 1, 2, 5, 7, 13, 21.
    * Skier heights: 3, 4, 7, 11, 18.
  """
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param num_skis;
param num_skiers;

param ski_heights{1..num_skis};
param skier_heights{1..num_skiers};

var x{1..num_skiers} >= 1 <= num_skis integer;
var z >= 0 <= 20 integer;

minimize obj: z;

# constraints

#
# MiniZinc version:
#    z = sum(i in 1..num_skiers) ( abs(ski_heights[x[i]] - skier_heights[i]) );
#
# Here we simulate element via the j index.
s.t. c1: z = sum{i in 1..num_skiers, j in 1..num_skis}
                     if j = x[i] then
                     abs(ski_heights[j] - skier_heights[i])
               ;

## Alternative approach
# s.t. c1b: z = sum{i in 1..num_skiers, j in 1..num_skis}
#                      (if x[i] = j then 1)*abs(ski_heights[j] - skier_heights[i])
#                ;


s.t. c2: alldiff{i in 1..num_skiers} x[i];

data;

param num_skis := 6;
param num_skiers := 5;

param ski_heights := 
         1 1 
         2 2 
         3 5 
         4 7 
         5 13 
         6 21;

param skier_heights := 
         1 3
         2 4 
         3 7 
         4 11 
         5 18;

option solver gecode;
option gecode_options 'var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=60';
solve;

display x;
display z;
