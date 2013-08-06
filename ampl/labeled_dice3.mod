/*

  Labeled dice / building blocks problem in AMPL.

  This is a general (symbolic) version of the two similar problems:
     http://www.hakank.org/ampl/labeled_dice2.mod
     http://www.hakank.org/ampl/building_blocks.mod

  Note: we don't do any symmetry breaking in this general model.


  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;
param num_words;
param words{1..num_words, 1..n} symbolic;

set letters;
param num_letters := card(letters);

param dice_card := num_letters / n;

# decision variables
var dice{letters} >= 1 <= n integer;

#
# constraints
#

# the letters in a word must be on a different die/block
s.t. c1{i in 1..num_words}: 
    alldiff{j in 1..n} dice[words[i,j]]
;

# there must be exactly $dice_card letters of each die/block
s.t. c2{i in 1..n}:
   exactly dice_card {j in letters} (dice[j] = i)
;

## symmetry breaking
## Note: This works only with the labeled_dice instance.
# s.t. c3:  
#    dice["a"] <= dice["b"] and
#    dice["b"] <= dice["c"] and
#    dice["c"] <= dice["m"] 
# ;


# The two problem instances:
# data labeled_dice.dat;
data building_blocks.dat;


# data;

# This is the labeled_dice.dat;
#
# param n := 4;
# param num_words := 13;

# # no "x" or "z"
# set letters = a b c d e f g h i j k l m n o p q r s t u v w y;

# param words: 1 2 3 4 :=
#  1  b u o y 
#  2  c a v e  
#  3  c e l t  
#  4  f l u b  
#  5  f o r k  
#  6  h e m p  
#  7  j u d y  
#  8  j u n k  
#  9  l i m n  
# 10  q u i p  
# 11  s w a g  
# 12  v i s a  
# 13  w i s h
# ;

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

