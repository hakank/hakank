/*

  Labeled dice problem in AMPL.

  From Jim Orlin "Colored letters, labeled dice: a logic puzzle"
  http://jimorlin.wordpress.com/2009/02/17/colored-letters-labeled-dice-a-logic-puzzle/
  """
  My daughter Jenn bough a puzzle book, and showed me a cute puzzle.  There 
  are 13 words as follows:  BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY, 
  JUNK, LIMN, QUIP, SWAG, VISA, WISH.

  There are 24 different letters that appear in the 13 words.  The question 
  is:  can one assign the 24 letters to 4 different cubes so that the 
  four letters of each word appears on different cubes.  (There is one 
  letter from each word on each cube.)  It might be fun for you to try 
  it.  I'll give a small hint at the end of this post. The puzzle was 
  created by Humphrey Dudley.
  """

  For a neater symbolic version see 
    http://www.hakank.org/ampl/labeled_dice2.mod


  There are 24 solutions (without symmetry breaking).
  With symmetry breaking, this is the unique solution:

  dice: 1, 2, 4, 2, 2, 4, 2, 1, 2, 1, 2, 1, 3, 4, 1, 4, 1, 3, 4, 3, 3, 3, 3, 4, 

  Die 1: A H J L O Q 
  Die 2: B D E G I K 
  Die 3: M R T U V W 
  Die 4: C F N P S Y 


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param num_words;
param words{1..num_words, 1..n} symbolic;

var dice{1..24} >= 1 <= n integer;

#
# constraints
#

# the letters in a word must be on a different die
s.t. c1{i in 1..num_words}: 
    alldiff{j in 1..n} dice[words[i,j]]
;

# there must be exactly 6 letters of each die
s.t. c2{i in 1..n}:
    exactly 6 {j in 1..24} (dice[j] = i)
;

## symmetry breaking
s.t. c3:  dice[ 1] <= dice[ 7] and
          dice[ 7] <= dice[13] and
          dice[13] <= dice[19] 
;

data;

param n := 4;
param num_words := 13;

#
# Note that X is not used.
#
# param words: 1 2 3 4 :=
#  1  B U O Y 
#  2  C A V E  
#  3  C E L T  
#  4  F L U B  
#  5  F O R K  
#  6  H E M P  
#  7  J U D Y  
#  8  J U N K  
#  9  L I M N  
# 10  Q U I P  
# 11  S W A G  
# 12  V I S A  
# 13  W I S H
# ;

param words: 1 2 3 4 :=
 1  2 21 15 24 
 2  3 1 22 5  
 3  3 5 12 20  
 4  6 12 21 2  
 5  6 15 18 11  
 6  8 5 13 16  
 7  10 21 4 24
 8  10 21 14 11  
 9  12 9 13 14  
10  17 21 9 16  
11  19 23 1 7  
12  22 9 19 1  
13  23 9 19 8
;


option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";


solve;


display dice;

for{i in 1..24} printf "%d, ", dice[i];
printf "\n\n";
for{die in 1..n} {
  printf "Die %d: ", die;
  for{j in 1..24} {
     # Also adjust for "x" -> "y"
     if dice[j] = die then printf "%s ", gsub(char(64+j),"X","Y");
  }
  printf "\n";
}

