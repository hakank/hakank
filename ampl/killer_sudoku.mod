/*

  Killer Sudoku in AMPL+CP.

  http://en.wikipedia.org/wiki/Killer_Sudoku
  """
  Killer sudoku (also killer su doku, sumdoku, sum doku, addoku, or 
  samunamupure) is a puzzle that combines elements of sudoku and kakuro. 
  Despite the name, the simpler killer sudokus can be easier to solve 
  than regular sudokus, depending on the solver's skill at mental arithmetic; 
  the hardest ones, however, can take hours to crack.

  ...
  The objective is to fill the grid with numbers from 1 to 9 in a way that 
  the following conditions are met:

    * Each row, column, and nonet contains each number exactly once.
    * The sum of all numbers in a cage must match the small number printed 
      in its corner.
    * No number appears more than once in a cage. (This is the standard rule 
      for killer sudokus, and implies that no cage can include more 
      than 9 cells.)

  In 'Killer X', an additional rule is that each of the long diagonals 
  contains each number once.
  """

  Here we solve the problem from the Wikipedia page, also shown here
  http://en.wikipedia.org/wiki/File:Killersudoku_color.svg

  The solution is:
     2 1 5 6 4 7 3 9 8
     3 6 8 9 5 2 1 7 4
     7 9 4 3 8 1 6 5 2
     5 8 6 2 7 4 9 3 1
     1 4 2 5 9 3 8 6 7
     9 7 3 8 1 6 4 2 5
     8 2 1 7 3 9 5 4 6
     6 5 9 4 2 8 7 1 3
     4 3 7 1 6 5 2 8 9


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;
param reg := ceil(sqrt(n));
param num_segments;

param segments{1..n, 1..n};
param segment_sums{1..num_segments};

var x{1..n, 1..n} >= 1 <= n integer;


#
# constraints
#

# Latin square
s.t. c1{i in 1..n}: alldiff{j in 1..n} x[i,j]; # rows
s.t. c2{j in 1..n}: alldiff{i in 1..n} x[i,j]; # columns

# Regions
s.t. c3{i in 0..reg-1, j in 0..reg-1}: 
     alldiff{r in i*reg+1..i*reg+reg, c in j*reg+1..j*reg+reg} x[r,c];

# Handle the segments
s.t. c4{p in 1..num_segments}:
     segment_sums[p] = sum{i in 1..n, j in 1..n: segments[i,j] = p} x[i,j]
;

data;

param n := 9;

param num_segments := 29;

param segments: 1 2 3 4 5 6 7 8 9 :=
1   1  1  2  2  2  3  4  5  6 
2   7  7  8  8  3  3  4  5  6 
3   7  7  9  9  3 10 11 11  6 
4  13 14 14  9 15 10 11 12  6 
5  13 16 16 17 15 10 12 12 18 
6  19 16 20 17 15 21 22 22 18 
7  19 20 20 17 23 21 21 24 24 
8  19 25 26 23 23 27 27 24 24 
9  19 25 26 23 28 28 28 29 29 
;

param segment_sums :=
 1   3
 2  15
 3  22
 4   4
 5  16
 6  15
 7  25
 8  17
 9   9
10   8
11  20
12  17
13   6
14  14
15  17
16  13
17  20
18  12
19  27
20   6
21  20
22   6
23  10
24  14
25   8
26  16
27  15
28  13
29  17
;



option solver gecode;
option gecode_options "icl=dom var_branching=regret_min_max val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=1 logverbosity=0";

solve;

printf "x:\n";
for{i in 1..n} {
  for{j in 1..n} {
     printf "%2d ", x[i,j];
  }
  printf "\n";
}

printf "\n";
