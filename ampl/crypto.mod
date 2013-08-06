/*

  Crypto problem in AMPL+CP.

  This is the standard benchmark "crypto" problem.

  From GLPK:s model cryto.mod.
  """
  This problem comes from the newsgroup rec.puzzle.
  The numbers from 1 to 26 are assigned to the letters of the alphabet.
  The numbers beside each word are the total of the values assigned to
  the letters in the word (e.g. for LYRE: L, Y, R, E might be to equal
  5, 9, 20 and 13, or any other combination that add up to 47).
  Find the value of each letter under the equations:

  BALLET  45     GLEE  66     POLKA      59     SONG     61
  CELLO   43     JAZZ  58     QUARTET    50     SOPRANO  82
  CONCERT 74     LYRE  47     SAXOPHONE 134     THEME    72
  FLUTE   30     OBOE  53     SCALE      51     VIOLIN  100
  FUGUE   50     OPERA 65     SOLO       37     WALTZ    34

  Solution:
  A, B,C, D, E,F, G, H, I, J, K,L,M, N, O, P,Q, R, S,T,U, V,W, X, Y, Z
  5,13,9,16,20,4,24,21,25,17,23,2,8,12,10,19,7,11,15,3,1,26,6,22,14,18

  Reference:
  Koalog Constraint Solver <http://www.koalog.com/php/jcs.php>,
  Simple problems, the crypto-arithmetic puzzle ALPHACIPHER.
  """
 
  
  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param num_words;
param longest_word;
param words{1..num_words, 1..longest_word} symbolic default "-";
param word_sums{1..num_words};

set Letters;

var x{Letters} >= 1 <= card(Letters) integer;


#
# constraints
#
s.t. c1: alldiff{i in Letters} x[i];

s.t. c2{w in 1..num_words}:
  sum{i in 1..longest_word: words[w,i] != "-"} x[words[w,i]] = word_sums[w]
;

data;

set Letters := A B C D E F G H I J K L M N O P Q R S T U V W X Y Z;

param num_words := 20;
param longest_word := 9;
param words: 1 2 3 4 5 6 7 8 9 :=
 1   B A L L E T . . .
 2   C E L L O . . . .
 3   C O N C E R T . . 
 4   F L U T E . . . .
 5   F U G U E . . . .
 6   G L E E . . . . .
 7   J A Z Z . . . . .
 8   L Y R E . . . . .
 9   O B O E . . . . .
10   O P E R A . . . .
11   P O L K A . . . .
12   Q U A R T E T . .
13   S A X O P H O N E
14   S C A L E . . . .
15   S O L O . . . . .
16   S O N G . . . . .
17   S O P R A N O . . 
18   T H E M E . . . .
19   V I O L I N . . .
20   W A L T Z . . . .
;

param word_sums :=
 1  45
 2  43
 3  74
 4  30
 5  50
 6  66
 7  58
 8  47
 9  53
10  65
11  59
12  50
13 134
14  51
15  37
16  61
17  82
18  72
19 100
20  34
;


option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display x;

for{i in Letters} {
  printf "%d, ", x[i];
}
printf "\n";