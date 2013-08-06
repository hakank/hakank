%
% Word square in Minizinc
%
%  From http://en.wikipedia.org/wiki/Word_square
%  """
%  A word square is a special case of acrostic. It consists of a set of words, 
%  all having the same number of letters as the total number of words (the 
%  "order" of the square); when the words are written out in a square grid 
%  horizontally, the same set of words can be read vertically.
%  """
%
%  Compare with the following models:
%  * http://www.hakank.org/comet/word_square.co
%  * http://www.hakank.org/gecode/word_square.cpp
%  * http://www.hakank.org/choco/WordSquare.java
%  * http://www.hakank.org/minizinc/crosswords.co
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


include "globals.mzn";

int: word_len;
int: num_words;
array[1..num_words, 1..word_len] of 0..26: words;
% include "word_len3.mzn";
include "word_len4.mzn";
% include "word_len5.mzn"; % Note: for Gecode/fz use -c-d 10000
% include "word_len6.mzn";
% include "word_len7.mzn";
% include "word_len8.mzn";


%
% Since Minizinc don't handle strings we use ints to represent the characters.
%
int: a =  1; int: b =  2; int: c =  3; int: d =  4; int: e =  5; int: f =  6;
int: g =  7; int: h =  8; int: i =  9; int: j = 10; int: k = 11; int: l = 12;
int: m = 13; int: n = 14; int: o = 15; int: p = 16; int: q = 17; int: r = 18;
int: s = 19; int: t = 20; int: u = 21; int: v = 22; int: w = 23; int: x = 24;
int: y = 25; int: z = 26; 

array[1..word_len] of var 1..num_words: E; 

array[1..26] of string: str = ["a","b","c","d","e","f","g","h","i","j","k","l","m",
                               "n","o","p","q","r","s","t","u","v","w","x","y","z"];

constraint

  %
  % check the overlapping positions
  % 
  forall(I, J in 1..word_len) (
    words[E[I], J] = words[E[J],I] % :: domain
  )
 
  /\ % redundant constraint
  all_different(E) :: domain

;

solve :: int_search(E, first_fail, indomain_min, complete) satisfy;
% solve satisfy;

output 
% [
%        "a =  1 b =  2 c =  3 d =  4 e =  5 f =  6\n",
%        "g =  7 h =  8 i =  9 j = 10 k = 11 l = 12\n",
%        "m = 13 n = 14 o = 15 p = 16 q = 17 r = 18\n",
%        "s = 19 t = 20 u = 21 w = 22 v = 23 x = 24\n",
%        "y = 25 z = 26\n\n", 
%
% ]
% ++ 
% [
%    "E" ++ show(I) ++ ": " ++ show(E[I]) ++ "  = " ++ show([words[E[I],J] | J in 1..word_len]) ++ "\n"
%     | I in 1..word_len
% ] ++ ["\n"];

[
   "E" ++ show(I) ++ ": " ++ show_int(5, E[I]) ++ "  = " ++ show(join("", [str[fix(words[E[I],J])] | J in 1..word_len])) ++ "\n"
    | I in 1..word_len
] ++ ["\n"];
 
