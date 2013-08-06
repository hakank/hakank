% 
% Best shuffle in MiniZinc.
% 
% From Rosetta Code
% http://rosettacode.org/wiki/Best_shuffle:
% """
% Shuffle the characters of a string in such a way that as many of 
% the character values are in a different position as possible. Print 
% the result as follows: original string, shuffled string, (score). 
% The score gives the number of positions whose character value did not change.
%
% For example: tree, eetr, (0)
%
% A shuffle that produces a randomized result among the best choices 
% is to be preferred. A deterministic approach that produces the 
% same sequence every time is acceptable as an alternative.
% 
% The words to test with are: abracadabra, seesaw, elk, grrrrrr, up, a 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_letters = 26;
int: a =  1; int: b =  2; int: c =  3; int: d =  4; int: e =  5; int: f =  6;
int: g =  7; int: h =  8; int: i =  9; int: j = 10; int: k = 11; int: l = 12;
int: m = 13; int: n = 14; int: o = 15; int: p = 16; int: q = 17; int: r = 18;
int: s = 19; int: t = 20; int: u = 21; int: v = 22; int: w = 23; int: x = 24;
int: y = 25; int: z = 26; 

array[1..num_letters] of string: letters = 
       ["a","b","c","d","e","f","g","h","i","j","k","l","m",
        "n","o","p","q","r","s","t","u","v","w","x","y","z"
    ];


% There are 374400 solutions with num_same = 0 (i.e. no fixpoint)
% int: len = 11;
% array[1..len] of int: word = [a,b,r,a,c,a,d,a,b,r,a];

% seesaw: There are 116 different solutions with num_same = 0
% int: len = 6;
% array[1..len] of int: word = [s,e,e,s,a,w];

% elk: There are 2 solutions with num_same = 0
% int: len = 3;
% array[1..len] of int: word = [e,l,k];

% grrrrrr: There are 4320 solutions with num_same = 5 (minimum)
% int: len = 7;
% array[1..len] of int: word = [g,r,r,r,r,r,r];

% up: there is one solution with num_same = 0
% int: len = 2;
% array[1..len] of int: word = [u,p];

% a: one solution with num_same = 1
% int: len = 1;
% array[1..len] of int: word = [a];

% aabbbbaa 
% (From http://rosettacode.org/wiki/Talk:Best_shuffle)
% There are 576 no fixpoint solutions.
int: len = 8;
array[1..len] of int: word = [a,a,b,b,b,b,a,a];

% decision variables
array[1..len] of var 1..len: shuffle;
var 0..len: num_same;

% solve minimize num_same;
solve :: int_search(shuffle, first_fail, indomain_min, complete) satisfy;

constraint
   alldifferent(shuffle) 
   /\
   num_same = sum(I in 1..len) ( bool2int( word[shuffle[I]] = word[I]) )
   /\
   num_same = 0
   % num_same = 5 % for grrrrrr
;

output [
  show(letters[word[I]])
  | I in 1..len
] 
++ 
[", "]
++
[
  show(letters[word[fix(shuffle[I])]])
  | I in 1..len
] 
++ [", (" ++ show(num_same) ++ ")\n"]
;

