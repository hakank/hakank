/*

  Minesweeper problem in AMPL+CP.

  From gecode/examples/minesweeper.cc:
  """
  A specification is a square matrix of characters. Alphanumeric characters represent
  the number of mines adjacent to that field. Dots represent fields with an unknown number
  of mines adjacent to it (or an actual mine).
  """
  
  E.g.
       "..2.3."
       "2....."
       "..24.3"
       "1.34.."
       ".....3"
       ".3.3.."
  """
  
  Also see 
   
  http://www.janko.at/Raetsel/Minesweeper/index.htm
   (the first 10 examples are from)

  http://en.wikipedia.org/wiki/Minesweeper_(computer_game)

  Ian Stewart on Minesweeper: http://www.claymath.org/Popular_Lectures/Minesweeper/

  Richard Kaye's Minesweeper Pages
  http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.htm
  Some Minesweeper Configurations
  http://web.mat.bham.ac.uk/R.W.Kaye/minesw/minesw.pdf


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param r; # rows
param c; # columns

set t = {-1..1};

# Note: the default -1 is the unknown value
param game{1..r, 1..c} integer default -1;

var mines{1..r, 1..c} binary;

# minimize obj: 1;


#
# For each cell in game that is >= 0: 
#
# The number in the cell should be the number of 
# all the neighboring cells that contains a mine.
#
s.t. c1{i in 1..r, j in 1..c: game[i,j] >= 0}:
            game[i,j] = sum{a in -1..1, b in -1..1:
                            i+a > 0 and j+b > 0 and
                            i+a <= r and j+b <= c} (mines[i+a, j+b]);

#
# If there's a count in a cell, 
# then it can't be a mine.
#
s.t. c2{i in 1..r, j in 1..c}:
       game[i,j] >= 0 ==> mines[i,j] = 0;

# Alternative version, not using ==>
# s.t. c2{i in 1..r, j in 1..c: game[i,j] >= 0}: 
#    mines[i,j] = 0;


#
# Redundant constraint: 
#   If a cell contains a mine then
#   the cell in game must be unknown (e.g. -1).
#
#s.t. c3{i in 1..r, j in 1..c}: 
#  mines[i,j] = 1 ==> game[i,j] < 0;


# data minesweeper0.dat;
# data minesweeper1.dat;
# data minesweeper2.dat;
# data minesweeper3.dat;
# data minesweeper4.dat;
# data minesweeper5.dat;
# data minesweeper6.dat;
# data minesweeper7.dat;
# data minesweeper8.dat;
# data minesweeper9.dat;
# data minesweeper_basic3.dat;
# data minesweeper_basic4.dat;
# data minesweeper_basic4x4.dat;
# data minesweeper_config_page2.dat;
# data minesweeper_config_page3.dat;
# data minesweeper_german_lakshatov.dat;
# data minesweeper_splitter.dat;
# data minesweeper_wire.dat;


data;

#
# This is the same as in minesweeper0.dat .
#
# param r := 6;
# param c := 6;
# param game:  1  2  3  4  5  6 :=
#    1        . . 2 . 3 . 
#    2        2 . . . . . 
#    3        . . 2 4 . 3 
#    4        1 . 3 4 . . 
#    5        . . . . . 3 
#    6        . 3 . 3 . .
# ;

# This is the same as minesweeper1.dat
#
param r := 8;
param c := 8;
param game:  1  2  3  4  5  6  7  8 :=
   1         . 2 . 2 1 1 . . 
   2         . . 4 . 2 . . 2 
   3         2 . . 2 . . 3 . 
   4         2 . 2 2 . 3 . 3 
   5         . . 1 . . . 4 . 
   6         1 . . . 2 . . 3 
   7         . 2 . 2 2 . 3 . 
   8         1 . 1 . . 1 . 1
;



option solver gecode;
# option solver cplex;

solve;
# write gtest;

# display game;

print "Game:";
for{i in 1..r} {
     for{j in 1..c} {
        if game[i,j] >= 0 then {
           printf "%d ", game[i,j];
        } else {
           printf ". ";
        }
     }
     printf "\n";
}

printf "\n";

display mines;

for{i in 1..r} {
     for{j in 1..c} {
        if mines[i,j] = 1 then
           printf "X ";
        else 
           printf ". ";
     }
     printf "\n";
}
