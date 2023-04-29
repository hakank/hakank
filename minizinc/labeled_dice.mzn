% 
% Labeled dice in MiniZinc.
% 
% From Jim Orlin "Colored letters, labeled dice: a logic puzzle"
% http://jimorlin.wordpress.com/2009/02/17/colored-letters-labeled-dice-a-logic-puzzle/
%   """
%   My daughter Jenn bough a puzzle book, and showed me a cute puzzle.  There 
%   are 13 words as follows:  BUOY, CAVE, CELT, FLUB, FORK, HEMP, JUDY, 
%   JUNK, LIMN, QUIP, SWAG, VISA, WISH.

%   There are 24 different letters that appear in the 13 words.  The question 
%   is:  can one assign the 24 letters to 4 different cubes so that the 
%   four letters of each word appears on different cubes.  (There is one 
%   letter from each word on each cube.)  It might be fun for you to try 
%   it.  I'll give a small hint at the end of this post. The puzzle was 
%   created by Humphrey Dudley.
%   """
% 
% Compare with the following models:
% * MiniZinc: http://www.hakank.org/minizinc/building_blocks.mzn
% 
% * Comet: http://www.hakank.org/comet/building_blocks.co
% * ECLiPSe: http://www.hakank.org/eclipse/building_blocks.ecl
% * SICStus Prolog: http://www.hakank.org/sicstus/building_blocks.pl
% * Gecode: http://www.hakank.org/gecode/building_blocks.cpp
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

int: n = 4;
int: num_words = 13;

enum letters = {A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,Y};

array[1..num_words, 1..n] of int: words = array2d(1..num_words, 1..n,
  [
   B,U,O,Y,
   C,A,V,E, 
   C,E,L,T, 
   F,L,U,B, 
   F,O,R,K, 
   H,E,M,P, 
   J,U,D,Y, 
   J,U,N,K, 
   L,I,M,N, 
   Q,U,I,P, 
   S,W,A,G, 
   V,I,S,A, 
   W,I,S,H
  ]);

% decision variable
array[1..24] of var 1..n: dice;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  % the letters in a word must be on a different die
  forall(i in 1..num_words) (
    alldifferent([dice[words[i,j]] | j in 1..n])
  )

  /\
  % there must be exactly 6 letters of each die
  forall(i in 1..n) (
    sum(j in 1..24) (dice[j] = i) = 6
  )

  /\
  % symmetry breaking
  dice[ 1] <= dice[ 7] /\
  dice[ 7] <= dice[13] /\
  dice[13] <= dice[19]

;

output
[
  "\(letters)\n\(dice)"
]
++
[
  if i == A then "\ndice: " ++ show(d) ++ ": " else "" endif ++
  if fix(dice[i]) == d then
    show(i) ++ " "
  else ""
  endif 
  | d in 1..4, i in letters
] ++ ["\n"];
