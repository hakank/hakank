/*

  Kakuro puzzle in AMPL+CP.

  http://en.wikipedia.org/wiki/Kakuro
  """
  The object of the puzzle is to insert a digit from 1 to 9 inclusive 
  into each white cell such that the sum of the numbers in each entry 
  matches the clue associated with it and that no digit is duplicated in 
  any entry. It is that lack of duplication that makes creating Kakuro 
  puzzles with unique solutions possible, and which means solving a Kakuro 
  puzzle involves investigating combinations more, compared to Sudoku in 
  which the focus is on permutations. There is an unwritten rule for 
  making Kakuro puzzles that each clue must have at least two numbers 
  that add up to it. This is because including one number is mathematically 
  trivial when solving Kakuro puzzles; one can simply disregard the 
  number entirely and subtract it from the clue it indicates.
  """

  This model solves the problem at the Wikipedia page. 
  For a larger picture, see
  http://en.wikipedia.org/wiki/File:Kakuro_black_box.svg

  The solution:
    9 7 0 0 8 7 9
    8 9 0 8 9 5 7
    6 8 5 9 7 0 0
    0 6 1 0 2 6 0
    0 0 4 6 1 3 2
    8 9 3 1 0 1 4
    3 1 2 0 0 2 1

  or rather

    9 7 _ _ 8 7 9
    8 9 _ 8 9 5 7
    6 8 5 9 7 _ _
    _ 6 1 _ 2 6 _
    _ _ 4 6 1 3 2
    8 9 3 1 _ 1 4
    3 1 2 _ _ 2 1


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param n;
param num_row_hints;
param num_col_hints;

param hints_row{1..n, 1..n} default -1;
param hints_col{1..n, 1..n} default -1;

param row_hint_sums{1..num_row_hints};
param col_hint_sums{1..num_col_hints};

# decision variables
var x{1..n, 1..n} >= 0 <= 9 integer;


#
# constraints
#

# Handle blanks
s.t. c1{i in 1..n, j in 1..n: hints_row[i,j] = -1}:
     x[i,j] = 0;

s.t. c1b{i in 1..n, j in 1..n: hints_row[i,j] >= 0}:
     x[i,j] > 0 
;

# Rows
s.t. c2{p in 1..num_row_hints}:
      row_hint_sums[p] = sum{i in 1..n,j in 1..n: hints_row[i,j] = p} x[i,j]
;

s.t. c2b{p in 1..num_row_hints}:
      alldiff{i in 1..n, j in 1..n: hints_row[i,j] = p} x[i,j]
;

# Columns
s.t. c3{p in 1..num_col_hints}:
      col_hint_sums[p] = sum{i in 1..n,j in 1..n: hints_col[i,j] = p+num_row_hints} x[i,j]
;
s.t. c3b{p in 1..num_col_hints}:
      (alldiff{i in 1..n, j in 1..n: hints_col[i,j] = p+num_row_hints} x[i,j])
;


data;

#
# The problem (without the operation)
#
# For a better view of the problem, see
#  http:%en.wikipedia.org/wiki/File:KenKenProblem.svg
#
param n := 7;
param num_row_hints := 12;
param num_col_hints := 12;

# The numbers are the "hint segment" a hint belongs to.
# The blanks (.) are also in these grids.

# The row hints
param hints_row: 1 2 3 4 5 6 7 :=

#   1  2  3  4  5  6  7
1   1  1  .  .  2  2  2 
2   3  3  .  4  4  4  4 
3   5  5  5  5  5  .  . 
4   .  6  6  .  7  7  . 
5   .  .  8  8  8  8  8 
6   9  9  9  9  . 10 10 
7  11 11 11  .  . 12 12 
;

# The column hints
param hints_col: 1 2 3 4 5 6 7 :=
# 1  2  3  4  5  6  7
1  13 15  .  . 20 21 23 
2  13 15  . 18 20 21 23 
3  13 15 17 18 20  .  . 
4   . 15 17  . 20 22  . 
5   .  . 17 19 20 22 24 
6  14 16 17 19  . 22 24 
7  14 16 17  .  . 22 24 
;

# sums for row hints
param row_hint_sums := 
 1  16  
 2  24  
 3  17  
 4  29  
 5  35  
 6   7  
 7   8  
 8  16  
 9  21  
10   5  
11   6  
12   3
;

# sums for column hints
param col_hint_sums :=
 1   23  
 2   11  
 3   30  
 4   10  
 5   15  
 6   17  
 7    7  
 8   27  
 9   12  
10   12  
11   16  
12    7
;


option solver gecode;
option gecode_options "icl=dom var_branching=regret_min_max val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=1 logverbosity=0";

solve;

printf "x:\n";
for{i in 1..n} {
  for{j in 1..n} {
     if x[i,j] = 0 then
        printf "_ ";
     else 
        printf "%d ", x[i,j];
  }
  printf "\n";
}

printf "\n";
