/*

  Survo puzzle in AMPL+CP.

  http://en.wikipedia.org/wiki/Survo_Puzzle
  """
  Survo puzzle is a kind of logic puzzle presented (in April 2006) and studied 
  by Seppo Mustonen. The name of the puzzle is associated to Mustonen's 
  Survo system which is a general environment for statistical computing and 
  related areas.
  
  In a Survo puzzle the task is to fill an m * n table by integers 1,2,...,m*n so 
  that each of these numbers appears only once and their row and column sums are 
  equal to integers given on the bottom and the right side of the table. 
  Often some of the integers are given readily in the table in order to 
  guarantee uniqueness of the solution and/or for making the task easier.
  """
  
  See also
  http://www.survo.fi/english/index.html
  http://www.survo.fi/puzzles/index.html

  References:
  Mustonen, S. (2006b). "On certain cross sum puzzles"
  http://www.survo.fi/papers/puzzles.pdf 
  Mustonen, S. (2007b). "Enumeration of uniquely solvable open Survo puzzles." 
  http://www.survo.fi/papers/enum_survo_puzzles.pdf 
  Kimmo Vehkalahti: "Some comments on magic squares and Survo puzzles" 
  http://www.helsinki.fi/~kvehkala/Kimmo_Vehkalahti_Windsor.pdf
  R code: http://koti.mbnet.fi/tuimala/tiedostot/survo.R


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param r;
param c;
param rowsums{1..r};
param colsums{1..c};
param matrix{1..r, 1..c};

var x{1..r, 1..c} >= 0 <= r*c integer;

#
# constraints
#
s.t. c1: alldiff{i in 1..r, j in 1..c} x[i,j];

s.t. c2{i in 1..r, j in 1..c: matrix[i,j] > 0}:
        matrix[i,j] = x[i,j];

s.t. c3{i in 1..r}: sum{j in 1..c} (x[i,j]) = rowsums[i];

s.t. c4{j in 1..c}: sum{i in 1..r} (x[i,j]) = colsums[j];


data survo_puzzle1.dat;
# data survo_puzzle2.dat; # Solution determined by presolve
# data survo_puzzle3.dat;
# data survo_puzzle4.dat;
# data survo_puzzle5.dat;
# data survo_puzzle6.dat;
# data survo_puzzle7.dat;

# data;

# survo_puzzle1.dat
# # http://en.wikipedia.org/wiki/Survo_Puzzle, first example
# param r := 3;
# param c := 4;
# param rowsums :=
#    1 30
#    2 18
#    3 30
# ;
# param colsums :=
#    1 27
#    2 16
#    3 10
#    4 25;
# param matrix: 1 2 3 4 :=
#    1  0 6 0 0
#    2  8 0 0 0
#    3  0 0 3 0 
# ;

option solver gecode;
#option gecode_options 'var_branching=size_min val_branching=min outlev=1 outfreq=1';

solve;

for{i in 1..r} {
   for{j in 1..c} {
      printf "%2d ", x[i,j];
   }
   printf "  %d\n", rowsums[i];
}

for{j in 1..c} {
    printf "%2d ", colsums[j];
}
printf "\n";