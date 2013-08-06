/*

  Fill-a-Pix problem in AMPL+CP.

  From http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
  """
  Each puzzle consists of a grid containing clues in various places. The 
  object is to reveal a hidden picture by painting the squares around each 
  clue so that the number of painted squares, including the square with 
  the clue, matches the value of the clue. 
  """

  Other names of this puzzle:

      * ぬり絵パズル
      * Nurie-Puzzle
      * Majipiku
      * Oekaki-Pix
      * Mosaic
      * Mosaik
      * Mozaïek
      * ArtMosaico
      * Count and Darken
      * Nampre puzzle
      * Komsu Karala!
      * Cuenta Y Sombrea
      * Mosaico
      * Voisimage
      * Magipic
      * Fill-In


  http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
  """
  Fill-a-Pix is a Minesweeper-like puzzle based on a grid with a pixilated 
  picture hidden inside. Using logic alone, the solver determines which 
  squares are painted and which should remain empty until the hidden picture 
  is completely exposed.
  """
  
  Fill-a-pix History:
  http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/history



  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param puzzle{1..n, 1..n} default -1;

var x{1..n, 1..n} binary;

#
# constraints
#
s.t. c1{i in 1..n, j in 1..n: puzzle[i,j] >= 0}:
    puzzle[i,j] = sum{a in {-1,0,1}, b in {-1,0,1}: 
                            i+a > 0  and j+b >  0 and
                            i+a <= n and j+b <= n}
                         x[i+a, j+b]
;

# data fill_a_pix1.dat;
data fill_a_pix2.dat;
# data fill_a_pix3.dat;

# data;

## This is the problem in fill_a_pix1.dat 
# param n := 10;
# param puzzle: 1 2 3 4 5 6 7 8 9 10 :=
#  1  . . . . . . . . 0 . 
#  2  . 8 8 . 2 . 0 . . . 
#  3  5 . 8 . . . . . . . 
#  4  . . . . . 2 . . . 2 
#  5  1 . . . 4 5 6 . . . 
#  6  . 0 . . . 7 9 . . 6 
#  7  . . . 6 . . 9 . . 6 
#  8  . . 6 6 8 7 8 7 . 5 
#  9  . 4 . 6 6 6 . 6 . 4 
# 10  . . . . . . 3 . . .
# ;


option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=60";

# option solver ilogcp;

solve;
# write gtest;

for {i in 1..n} {
   for {j in 1..n} {
       if x[i,j] = 1 then {
          printf "#";
       } else {
          printf " ";
       }
   }
   printf "\n";
};
printf "\n";

