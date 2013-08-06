%
% Cross word in Minizinc
%
% This is a standard example for constraint logic programming. See e.g.
%
% http://www.cis.temple.edu/~ingargio/cis587/readings/constraints.html
% """
% We are to complete the puzzle
%
%      1   2   3   4   5
%    +---+---+---+---+---+       Given the list of words:
%  1 | 1 |   | 2 |   | 3 |             AFT     LASER
%    +---+---+---+---+---+             ALE     LEE
%  2 | # | # |   | # |   |             EEL     LINE
%    +---+---+---+---+---+             HEEL    SAILS
%  3 | # | 4 |   | 5 |   |             HIKE    SHEET
%    +---+---+---+---+---+             HOSES   STEER
%  4 | 6 | # | 7 |   |   |             KEEL    TIE
%    +---+---+---+---+---+             KNOT
%  5 | 8 |   |   |   |   |
%    +---+---+---+---+---+       
%  6 |   | # | # |   | # |       The numbers 1,2,3,4,5,6,7,8 in the crossword
%    +---+---+---+---+---+       puzzle correspond to the words 
%                                                  that will start at those locations.
% """

% The model was inspired by Sebastian Brand's Array Constraint cross word example
% http://www.cs.mu.oz.au/~sbrand/project/ac/
% http://www.cs.mu.oz.au/~sbrand/project/ac/examples.pl
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%


%
% Since Minizinc don't (yet?) handle strings we use ints to represent the characters.
%
int: a =  1; int: b =  2; int: c =  3; int: d =  4; int: e =  5; int: f =  6;
int: g =  7; int: h =  8; int: i =  9; int: j = 10; int: k = 11; int: l = 12;
int: m = 13; int: n = 14; int: o = 15; int: p = 16; int: q = 17; int: r = 18;
int: s = 19; int: t = 20; int: u = 21; int: v = 22; int: w = 23; int: x = 24;
int: y = 25; int: z = 26; 

% The domains (possible positions in the cross word)
var 1..5:   E1;
var 1..5:   E2;
var 1..5:   E3;
var 6..10:  E4;
var 6..10:  E5;
var 11..15: E6;
var 11..15: E7;
var 1..5:   E8;

array[1..15,1..5] of 0..26: A; % Array of the possible words

% Definition of the words. A zero is used to fill the row.
A = array2d(1..15, 1..5, [
  h, o, s, e, s, %  HOSES
  l, a, s, e, r, %  LASER
  s, a, i, l, s, %  SAILS
  s, h, e, e, t, %  SHEET
  s, t, e, e, r, %  STEER
  h, e, e, l, 0, %  HEEL
  h, i, k, e, 0, %  HIKE
  k, e, e, l, 0, %  KEEL
  k, n, o, t, 0, %  KNOT
  l, i, n, e, 0, %  LINE
  a, f, t, 0, 0, %  AFT
  a, l, e, 0, 0, %  ALE
  e, e, l, 0, 0, %  EEL
  l, e, e, 0, 0, %  LEE
  t, i, e, 0, 0  %  TIE
]);



% The overlapping positions in the cross word, i.e.
% where the letters is the same
constraint 
   A[E1, 3] = A[E2, 1] /\  %  s
   A[E1, 5] = A[E3, 1] /\  %  s

   A[E4, 2] = A[E2, 3] /\  %  i
   A[E4, 3] = A[E5, 1] /\  %  k
   A[E4, 4] = A[E3, 3] /\  %  e

   A[E7, 1] = A[E2, 4] /\  %  l
   A[E7, 2] = A[E5, 2] /\  %  e
   A[E7, 3] = A[E3, 4] /\  %  e

   A[E8, 1] = A[E6, 2] /\  %  l
   A[E8, 3] = A[E2, 5] /\  %  s
   A[E8, 4] = A[E5, 3] /\  %  e
   A[E8, 5] = A[E3, 5]     %  r
;

% solve :: int_search([E1,E2,E3,E4,E5,E6,E7,E8], "first_fail", "indomain", "complete") satisfy;
solve satisfy;

%  The result:
%
%   E1 =  1  HOSES
%   E2 =  3  SAILS
%   E3 =  5  STEER
%   E4 =  7  HIKE
%   E5 =  8  KEEL
%   E6 = 12  ALE
%   E7 = 14  LEE
%   E8 =  2  LASER


output [
       "a =  1 b =  2 c =  3 d =  4 e =  5 f =  6\n",
       "g =  7 h =  8 i =  9 j = 10 k = 11 l = 12\n",
       "m = 13 n = 14 o = 15 p = 16 q = 17 r = 18\n",
       "s = 19 t = 20 u = 21 w = 22 v = 23 x = 24\n",
       "y = 25 z = 26\n\n", 

      "E1: ", show(E1), " ", show([A[E1,i] | i in 1..5]), "\n",  % HOSES
      "E2: ", show(E2), " ", show([A[E2,i] | i in 1..5]), "\n",  % SAILS
      "E3: ", show(E3), " ", show([A[E3,i] | i in 1..5]), "\n",  % STEER
      "E4: ", show(E4), " ", show([A[E4,i] | i in 1..5]), "\n",  % HIKE
      "E5: ", show(E5), " ", show([A[E5,i] | i in 1..5]), "\n",  % KEEL
      "E6: ", show(E6), " ", show([A[E6,i] | i in 1..5]), "\n",  % ALE
      "E7: ", show(E7), " ", show([A[E7,i] | i in 1..5]), "\n",  % LEE
      "E8: ", show(E8), " ", show([A[E8,i] | i in 1..5]), "\n",  % LASER

] ++ ["\n"]
;
