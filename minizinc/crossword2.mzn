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
% Result:
%
% E1: 1 [8, 15, 19, 5, 19]     % MOSES
% E2: 3 [19, 1, 9, 12, 19]     % SAILS
% E3: 5 [19, 20, 5, 5, 18]     % STEER
% E4: 7 [8, 9, 11, 5, 0]       % HIKE
% E5: 8 [11, 5, 5, 12, 0]      % KEEL
% E6: 12 [1, 12, 5, 0, 0]      % ALE 
% E7: 14 [12, 5, 5, 0, 0]      % LEE 
% E8: 2 [12, 1, 19, 5, 18]     % LASER


include "globals.mzn";

%
% Since Minizinc don't handle strings we use ints to represent the characters.
%
int: a =  1; int: b =  2; int: c =  3; int: d =  4; int: e =  5; int: f =  6;
int: g =  7; int: h =  8; int: i =  9; int: j = 10; int: k = 11; int: l = 12;
int: m = 13; int: n = 14; int: o = 15; int: p = 16; int: q = 17; int: r = 18;
int: s = 19; int: t = 20; int: u = 21; int: v = 22; int: w = 23; int: x = 24;
int: y = 25; int: z = 26; 

int: N = 8;
array[1..N] of var 1..15: E; 

%
% The overlapping positions in the crossword, i.e.
% where the letters is the same
%
int: num_overlapping = 12;
array[1..num_overlapping, 1..4] of int: overlapping = array2d(1..num_overlapping, 1..4,
[
 1, 3, 2, 1, 
 1, 5, 3, 1, 
 
 4, 2, 2, 3, 
 4, 3, 5, 1, 
 4, 4, 3, 3, 
 
 7, 1, 2, 4, 
 7, 2, 5, 2, 
 7, 3, 3, 4, 
 
 8, 1, 6, 2, 
 8, 3, 2, 5, 
 8, 4, 5, 3, 
 8, 5, 3, 5  
 ]);


% Definition of the words. A zero is used to fill the row.
array[1..15,1..5] of 0..26: A = array2d(1..15, 1..5, [
h, o, s, e, s,     %%  HOSES
l, a, s, e, r,     %%  LASER
s, a, i, l, s,     %%  SAILS
s, h, e, e, t,     %%  SHEET
s, t, e, e, r,     %%  STEER
h, e, e, l, 0,     %%  HEEL
h, i, k, e, 0,     %%  HIKE
k, e, e, l, 0,     %%  KEEL
k, n, o, t, 0,     %%  KNOT
l, i, n, e, 0,     %%  LINE
a, f, t, 0, 0,     %%  AFT
a, l, e, 0, 0,     %%  ALE
e, e, l, 0, 0,     %%  EEL
l, e, e, 0, 0,     %%  LEE
t, i, e, 0, 0      %%  TIE
]);



constraint

  %
  % check all overlapping positions
  % 
  forall(i in 1..num_overlapping) (
    A[E[overlapping[i,1]], overlapping[i,2]] =  A[E[overlapping[i,3]], overlapping[i,4]]
  )
  /\ % redundant constraint
  all_different(E)

;

solve :: int_search(E, first_fail, indomain_median, complete) satisfy;


output [
       "a =  1 b =  2 c =  3 d =  4 e =  5 f =  6\n",
       "g =  7 h =  8 i =  9 j = 10 k = 11 l = 12\n",
       "m = 13 n = 14 o = 15 p = 16 q = 17 r = 18\n",
       "s = 19 t = 20 u = 21 w = 22 v = 23 x = 24\n",
       "y = 25 z = 26\n\n", 

] ++ [
   "E" ++ show(I) ++ ": " ++ show(E[I]) ++ "  = " ++ show([A[E[I],J] | J in 1..5]) ++ "\n"
   | I in 1..8
 ] ++ ["\n"];
 