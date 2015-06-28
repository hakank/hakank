% 
% Enigma 290: Dice with a difference in MiniZinc.
% 
% https://enigmaticcode.wordpress.com/2015/06/26/enigma-290-dice-with-a-difference/
% """
% From New Scientist #1438, 10th January 1985 [link]
%
%  Throwing two dice will give you a number from 2 to 12. Of course, some numbers are 
%  more likely that others. The probability of 2 for instance is 1/36; of 3 is 2/36; 
%  of 4 is 3/36; …; of 7 is 6/36; …; of 8 is 5/36; …; of 12 is 1/36.
%
%  That is true of two ordinary 6-sided dice, each bearing the letters of ENIGMA 
%  (which stand for the numbers one to six).
% 
%  It is also true of this special pair of dice I have made — one with 9 sides bearing 
%  the letters IMAGINING, the other with 4 sides bearing the letters of GAGS. 
%  (S is a positive integer).
%
%  I’m not going to tell you how I constructed 9-sided and 4-sided dice. But I did, and they 
%  are fair dice. Can you interpret the MEANINGS of these fascinating facts?
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 9; % first die, 9 sides, IMAGINING
int: f = 4; % second die, 4 sides, GAGS
int: e = 6; % ENIGMA dice, 6 sides

int: k = 12;
array[2..k] of int: probs = array1d(2..k, [1,2,3,4,5,6,5,4,3,2,1]);

% decision variables
var 1..k: I;
var 1..k: M;
var 1..k: A;
var 1..k: G;
var 1..k: N;
var 1..k: S;
var 1..e: E;

array[1..n] of var int: dice9 = [I,M,A,G,I,N,I,N,G];
array[1..f] of var int: dice4 = [G,A,G,S];
array[1..e] of var int: dice6 = [E,N,I,G,M,A];

solve satisfy;

constraint
 forall(p in 2..k) (
   probs[p] = sum(i in 1..n, j in 1..f) ( dice9[i]+dice4[j] == p)
   /\
   probs[p] = sum(i,j in 1..e) ( dice6[i]+dice6[j] == p)
 )
;

output [
  "dice9 (IMAGINING): ", show(dice9), "\n",
  "dice4 (GAGS): ", show(dice4), "\n",
  "dice6 (ENIGMA): ", show(dice6), "\n",
  "MEANINGS: ", show([M,E,A,N,I,N,G,S]), "\n"
];

