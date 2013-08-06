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

  This is a symbolic version of 
     http://www.hakank.org/ampl/labeled_dice.mod


  There are 24 solutions (without symmetry breaking).
  With symmetry breaking of ordering the first letter in each word, 
  this is the unique solution:

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

set letters;

# decision variables
var dice{letters} >= 1 <= n integer;

#
# constraints
#

# the letters in a word must be on a different die
s.t. c1b{i in 1..num_words}: 
    alldiff{j in 1..n} dice[words[i,j]]
;

# there must be exactly 6 letters of each die
s.t. c2b{i in 1..n}:
   exactly 6 {j in letters} (dice[j] = i)
;

## symmetry breaking
## Note: This works only with the labeled_dice instance.
# s.t. c3:  
#    dice["a"] <= dice["b"] and
#    dice["b"] <= dice["c"] and
#    dice["c"] <= dice["m"] 
# ;


data;

param n := 4;
param num_words := 13;

# no "x" or "z"
set letters = a b c d e f g h i j k l m n o p q r s t u v w y;

param words: 1 2 3 4 :=
 1  b u o y 
 2  c a v e  
 3  c e l t  
 4  f l u b  
 5  f o r k  
 6  h e m p  
 7  j u d y  
 8  j u n k  
 9  l i m n  
10  q u i p  
11  s w a g  
12  v i s a  
13  w i s h
;

option show_stats 2;

option solver gecode;
option gecode_options "icl=def var_branching=size_min val_branching=min outlev=1 outfreq=1 timelimit=30";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";


solve;


display dice;

for{i in letters} printf "%d, ", dice[i];
printf "\n\n";
for{die in 1..n} {
  printf "Die %d: ", die;
  for{j in letters} {
     if dice[j] = die then printf "%s ", j;
  }
  printf "\n";
}

