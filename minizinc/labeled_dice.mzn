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
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

int: n = 4;
set of int: Cube = 1..n;
int: num_words = 13;

int: A = 1;
int: B = 2;
int: C = 3;
int: D = 4;
int: E = 5;
int: F = 6;
int: G = 7;
int: H = 8;
int: I = 9;
int: J = 10;
int: K = 11;
int: L = 12;
int: M = 13;
int: N = 14;
int: O = 15;
int: P = 16;
int: Q = 17;
int: R = 18;
int: S = 19;
int: T = 20;
int: U = 21;
int: V = 22;
int: W = 23;
int: Y = 24;

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
array[1..24] of var Cube: dice;

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
    sum(j in 1..24) (bool2int(dice[j] = i)) = 6
  )

  % /\
  % % symmetry breaking
  % dice[ 1] <= dice[ 7] /\
  % dice[ 7] <= dice[13] /\
  % dice[13] <= dice[19]

;

output
[
  show(dice)
];
