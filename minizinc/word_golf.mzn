% 
% Word golf in MiniZinc.
% 
% http://en.wikipedia.org/wiki/Word_golf
% """
% Word golf (also called a word chain) is a game, a version of Word Ladders, 
% in which one word is turned into another through a process of substituting 
% single letters. A new English word must be formed each time a letter is 
% replaced, and players score points according to the number of steps taken. As 
% in regular golf, the player with the lowest score at the end of the game wins.
% """
% 

% Note: 
% This model assumes that the length of the ladder is fixed.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
% include "word_golf_n3.mzn"; % the words
% include "word_golf_n4.mzn"; % the words


int: num_words;
int: word_len;
array[1..word_len] of 1..26: start; % start word
array[1..word_len] of 1..26: end;   % end word
array[1..num_words, 1..word_len] of var 1..26: words;

int: ladder_len; % length of ladder
array[1..ladder_len, 1..word_len] of var 1..26: x;
array[1..ladder_len] of var 1..num_words: ladder_num;

% solve satisfy;
solve :: int_search(
        [x[i,j] | i in 1..ladder_len, j in 1..word_len], 
        first_fail, 
        indomain_min, 
        complete) 
    satisfy;

constraint

   % initialize the problem
   forall(i in 1..word_len) (
      x[1, i] = start[i]
      /\
      x[ladder_len, i] = end[i]
   )

   /\ % assign word j to the ladder position i
   forall(i in 1..ladder_len) (
      let {
        var 1..num_words: j
      }
      in

      % all words in x must be a word in words
      forall(k in 1..word_len) (
                               x[i,k] = words[j,k]   
      )
      /\ 
      ( % assign the specific word
        ladder_num[i] = j <-> 
        forall(k in 1..word_len) (
                               x[i,k] = words[j,k]   
        )
      )
   )
   /\
   all_different(ladder_num) :: domain

   /\ % just one difference is allowed between the words
   forall(i in 2..ladder_len) (
      sum(j in 1..word_len) ( 
         bool2int(
            x[i-1, j] != x[i, j]
         )
      ) = 1
     
   )
;


% example from the Wikipedia article: lass -> male
% start = [12,1,19,19]; % lass
% end = [13,1,12,5]; % male
% num_words = 5;
% word_len = 4;
% ladder_len = 5;
% words = array2d(1..num_words, 1..word_len, [
%        12,1,19,19, % lass
%        13,1,12,5, % male
%        13,1,18,5, % mare
%        13,1,18,19, % mars
%        13,1,19,19, % mass
% ]);

% example from the Wikipedia article: right -> wrong
% start = [18,9,7,8,20]; % right
% end = [23,18,15,14,7]; % wrong
% num_words = 13;
% ladder_len = 13;
% word_len = 5;
% words = array2d(1..num_words, 1..word_len, [
%         2,15,9,14,7, % boing
%         2,18,9,14,7, % bring
%         12,9,15,14,19, % lions
%         12,15,9,14,7, % loing
%         12,15,9,14,19, % loins
%         12,15,15,14,19, % loons
%         18,9,7,8,20, % right
%         19,9,7,8,19, % sighs
%         19,9,7,8,20, % sight
%         19,9,7,14,19, % signs
%         19,9,15,14,19, % sions
%         23,18,9,14,7, % wring
%         23,18,15,14,7, % wrong
% ]);


% work -> play
% (added some words)
start = [23,15,18,11]; % work
end = [16,12,1,25]; % play
ladder_len = 14;
num_words = 19;
word_len = 4;
% include "word_golf_n4.mzn"; % contains 10630 words. Takes too long...
words = array2d(1..num_words, 1..word_len, [
        6,5,1,18, % fear
        6,5,1,20, % feat
        6,12,1,20, % flat
        6,12,1,25, % flay
        6,15,18,5, % fore
        6,15,18,11, % fork
        16,1,18,5, % pare
        16,1,18,11, % park
        16,5,1,11, % peak
        16,5,1,18, % pear
        16,5,18,11, % perk
        16,12,1,25, % play
        16,15,18,5, % pore
        23,15,18,11, % work

        % these were added to complicate the problem
        12,1,19,19, % lass
        13,1,12,5, % male
        13,1,18,5, % mare
        13,1,18,19, % mars
        13,1,19,19, % mass
]);


output 
[
  "\nladder_num: ", show(ladder_num),
]
++
[
  if j = 1 then "\n" else " " endif ++
     show(x[i,j])
  | i in 1..ladder_len, j in 1..word_len
] ++ ["\n"];