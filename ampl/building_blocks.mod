/*

  Building Blocks puzzle (Dell Logic Puzzles) in AMPL.

  From http://brownbuffalo.sourceforge.net/BuildingBlocksClues.html
    """
    Each of four alphabet blocks has a single letter of the alphabet on each 
    of its six sides. In all, the four blocks contain every letter but 
    Q and Z. By arranging the blocks in various ways, you can spell all of 
    the words listed below. Can you figure out how the letters are arranged 
    on the four blocks?

    BAKE ONYX ECHO OVAL
    GIRD SMUG JUMP TORN
    LUCK VINY LUSH WRAP
    """


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

# symmetry breaking
# s.t. c3:  
#    dice["a"] <= dice["b"] and
#    dice["b"] <= dice["e"] and
#    dice["e"] <= dice["h"] 
# ;


data;

param n := 4;
param num_words := 12;

# no q or z
set letters = a b c d e f g h i j k l m n o p r s t u v w x y;

param words: 1 2 3 4 :=
 1  b a k e 
 2  o n y x 
 3  e c h o 
 4  o v a l 
 5  g i r d 
 6  s m u g 
 7  j u m p 
 8  t o r n 
 9  l u c k 
10  v i n y 
11  l u s h 
12  w r a p
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

